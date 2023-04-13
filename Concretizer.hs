
module Concretizer where

import qualified Data.Map.Lazy as M

import Abstract
    ( ColumnInfo(ColumnInfo),
      Data(..),
      DataType(IntType, StringType),
      EvalError,
      FieldSpecifier(Explicit, Implicit),
      Predicate,
      Query(..),
      Statement(..),
      TableCommand(..),
      TableName(..) )
import Tuple
    ( Tuple(..), getFields, getTupFieldNames, aliasTupFieldNames )
import Table ( getFieldNames, getScheme, getTableName, Table(..) )
import Field ( Field(..), FieldType(..) )
import File ( DbFile(File) )
import Pred
    ( AliasError,
      bindAliases,
      toNNF,
      toCNFloop,
      separate,
      typeCheck,
      evalPred )

type ConcretizationResult = Either ConcretizationError ConcreteStatement 

type ConcreteStatement = Either ConcreteCommand ConcreteQuery

type ConcretizationError = String


data ConcreteCommand
    = C Table
    | D Table
    | I Table Tuple
    | U Table PlanNode (Tuple -> Tuple)
    | Del Table PlanNode

newtype ConcreteQuery = Q PlanNode

data PlanNode
    = ReadTable Table
    | Projection PlanNode
    | Filter (Tuple -> Either EvalError Bool) PlanNode
    | Join PlanNode PlanNode
    | Output

instance Show PlanNode where
    show (ReadTable t) = "ReadTable (" ++ getTableName t ++ ")"
    show (Projection node) = "Projection(" ++ show node ++ ")"
    show (Filter _ node) = "Filter(" ++ show node ++ ")"
    show (Join n1 n2) = "Join(" ++ show n1 ++ ", " ++ show n2 ++ ")"
    show Output = "Output"

concretize :: M.Map String Table -> Statement -> ConcretizationResult
concretize tMap (Query q) = 
    case concretizeQuery tMap q of 
        Left e -> Left e
        Right q -> Right $ Right q

concretize tMap (TableCommand com) = 
    case concretizeCommand tMap com of
        Left e -> Left e
        Right c -> Right $ Left c

concretizeQuery :: M.Map String Table -> Query -> Either ConcretizationError ConcreteQuery
concretizeQuery tMap (SELECT colSel tables pred grBy) = 
    case buildAliasMap tMap tables of
        Left aliasError -> Left aliasError
        Right aMap -> case bindAliases aMap pred of
            Left aliasError -> Left aliasError
            Right aliasedPred -> case typeCheck (buildAliasedFieldToTypeMap aMap) aliasedPred of
                Just typeError -> Left typeError
                Nothing -> 
                    let finalPred = (toCNFloop . toNNF) aliasedPred in
                        Right $ Q $ makePlan aMap finalPred [] (orderJoins (M.toList aMap) finalPred)


concretizeCommand :: M.Map String Table -> TableCommand -> Either ConcretizationError ConcreteCommand 
concretizeCommand tMap (CREATE tName info) =
    if M.member tName tMap then
        Left "Table with that name already exists"
    else
        Right $ C $ Table tName (File fieldTypes (toFileName tName)) fieldNames fieldTypes 
        where 
            (fieldNames, fieldTypes) = unzip $ map repack info
            repack (ColumnInfo s (StringType len)) = (s, StringT len)
            repack (ColumnInfo s IntType) = (s, IntT)

concretizeCommand tMap (DROP tName) =
    case M.lookup tName tMap of
        Just table -> Right $ D table 
        Nothing -> Left "No table with that name found"

concretizeCommand tMap (INSERT tName fields values) = 
    case M.lookup tName tMap of
        Nothing                   -> Left "Table with that name doesn't exist"
        Just t@(Table _ _ fieldNames scheme) -> 
            let userFields = case fields of 
                    [] -> fieldNames
                    xs -> xs in
                if length userFields /= length values then
                    Left "Number of columns and values is not equal"
                else 
                    case toTuple (zip fieldNames scheme)(zip userFields values) of
                    Left error -> Left error
                    Right fs -> Right $ I t (Tuple (-1) scheme (Prelude.map Implicit fieldNames) fs)
        

concretizeCommand tMap (UPDATE tName info pred) = 
    case M.lookup tName tMap of
        Nothing -> Left "No table with that name found"
        Just table -> case buildAliasMap tMap [Single tName] of
                Left aliasError -> Left aliasError
                Right aMap -> case bindAliases aMap pred of
                    Left aliasError -> Left aliasError
                    Right aliasedPred -> case typeCheck (buildAliasedFieldToTypeMap aMap) aliasedPred of
                        Just typeError -> Left typeError
                        Nothing -> 
                            let finalPred = (toCNFloop . toNNF) aliasedPred in
                                case toTuple (zip (getFieldNames table) (getScheme table)) info of
                                    Left err -> Left err
                                    Right fields -> Right $ U table (makePlan aMap finalPred [] (orderJoins (M.toList aMap) finalPred)) (updateTuple fields)


concretizeCommand tMap (DELETE tName pred) =
    case M.lookup tName tMap of
        Nothing -> Left "No table with that name found"
        Just table -> case buildAliasMap tMap [Single tName] of
                Left aliasError -> Left aliasError
                Right aMap -> case bindAliases aMap pred of
                    Left aliasError -> Left aliasError
                    Right aliasedPred -> case typeCheck (buildAliasedFieldToTypeMap aMap) aliasedPred of
                        Just typeError -> Left typeError
                        Nothing -> 
                            let finalPred = (toCNFloop . toNNF) aliasedPred in
                                Right $ Del table $ makePlan aMap finalPred [] (orderJoins (M.toList aMap) finalPred)


toFileName :: String -> FilePath
toFileName s = "./tables/" ++ s

-- given list of pairs (fieldName, fieldType) and list of pairs (fieldName, value)
-- constructs a list of fields representing a tuple given to the insert function
-- returns either list of fields or string containing error message (e.g. user-specified 
-- column name that isn't present in the table scheme or wrong type of field given by the user) 
toTuple :: [(String, FieldType)] -> [(String, Data)] -> Either String [Field]
toTuple [] [] = Right []
toTuple [] ((uName, _) : _) = Left $ "Cannot recognize column name: " ++ show uName
toTuple ((fN, fT) : ts) ds = 
    case findAndDelete (\ (uName, _) -> fN == uName) ds of
        (Nothing, ds) -> case toTuple ts ds of -- no value given for this column
            Left furtherError  -> Left furtherError
            Right tuple -> Right $ Null : tuple
        (Just (_, value), ds) -> case toTuple ts ds of 
            Left furtherError -> Left furtherError
            Right tuple       -> case toField fT value of
                Left fieldError -> Left fieldError
                Right f    -> Right $ f : tuple 


-- find the first element of list satisfying given predicate
-- returns a pair of this element (or Nothing) and list with this element removed
findAndDelete :: (a -> Bool) -> [a] -> (Maybe a, [a])
findAndDelete _ [] = 
    (Nothing, [])
findAndDelete pred (x : xs) = 
    if pred x then
        (Just x, xs)
    else
        case findAndDelete pred xs of
            (res, xs) -> (res, x : xs)

toField :: FieldType -> Data -> Either String Field
toField (StringT len) (String s) = Right $ StringF $ take len s
toField IntT (Int x) = Right $ IntF x
toField (StringT _) (Int x) = Left $ "Expected string, but was given int: " ++ show x
toField IntT (String s) = Left $ "Expected int, but was given string: " ++ show s

updateTuple :: [Field] -> Tuple -> Tuple
updateTuple newFields toUpdate@(Tuple no scheme fNames fields) =
    Tuple no scheme fNames $ 
    zipWith 
    (\ nField oField -> case nField of
        Null -> oField
        f -> f) 
    newFields
    fields


buildAliasMap :: M.Map String Table -> [TableName] -> Either AliasError (M.Map String Table)
buildAliasMap tMap [] = Right M.empty
buildAliasMap tMap ((Single name)        : xs) = 
    case M.lookup name tMap of
        Nothing -> Left $ "Table " ++ show name ++ " not found"
        Just table -> case buildAliasMap tMap xs of
            Left err -> Left err
            Right aMap -> Right $ M.insert name table aMap
buildAliasMap tMap ((Aliased name alias) : xs) = 
    case M.lookup name tMap of 
        Nothing -> Left $ "Table " ++ show name ++ " not found"
        Just table -> case buildAliasMap tMap xs of
            Left err -> Left err
            Right aMap -> Right $ M.insert alias table aMap

buildAliasedFieldToTypeMap :: M.Map String Table -> M.Map (String, String) FieldType
buildAliasedFieldToTypeMap aMap = 
    Prelude.foldl
    (\ resMap (alias, table) ->
        Prelude.foldl
        (\ innerMap (fieldName, fieldType) -> M.insert (alias, fieldName) fieldType innerMap)
        resMap
        (zip (getFieldNames table) (getScheme table)))
    M.empty
    (M.toList aMap)

makePlan :: M.Map String Table -> Predicate -> [(String, Table)] -> [(String, Table)]-> PlanNode
makePlan aMap pred joined [] = Output -- should never happen
makePlan aMap pred joined [(alias, table)] = 
    let (predToApply, left) = separate pred (alias : map fst joined) in
        Filter (evaluablePred aMap (getTableName table, alias) predToApply) $ ReadTable table
makePlan aMap pred joined ((alias, table) : toJoin) = 
    let (predToApply, left) = separate pred (alias : map fst joined) in 
        let subPlan = makePlan aMap left ((alias, table) : joined) toJoin in
        Filter (evaluablePred aMap (getTableName table, alias) predToApply) $ Join subPlan (ReadTable table)

-- Optimalization here
orderJoins :: [(String, Table)] -> Predicate -> [(String, Table)]
orderJoins xs pred = xs

evaluablePred :: M.Map String Table -> (String, String) -> Predicate -> Tuple -> Either EvalError Bool
evaluablePred aMap (tableName, givenAlias) pred tup = 
    evalPred (buildFieldToValueMap (getAliasedFields aMap) (aliasTupFieldNames tup (tableName, givenAlias))) pred

buildFieldToValueMap :: [(String, String)] -> Tuple -> M.Map (String, String) Field 
buildFieldToValueMap xs tup =  
    M.fromList $
    Prelude.concatMap
    (\ (Explicit fAlias fName, field) ->
        Prelude.foldl
        (\ acc (fAlias2, fName2) ->
            if fAlias == fAlias2 && fName == fName2 then
                ((fAlias, fName), field) : acc
            else
                acc
            )
        []
        xs)
    $ zip (getTupFieldNames tup) (getFields tup)

getAliasedFields :: M.Map String Table -> [(String, String)]
getAliasedFields aMap = 
    concatMap
    (\ (alias, table) ->
        Prelude.map
        (\ (fieldName) -> (alias, fieldName))
        (getFieldNames table))
    (M.toList aMap)