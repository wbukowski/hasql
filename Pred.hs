{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Pred where

import Data.Map.Lazy as M


import Data.List (intercalate, isInfixOf)
import Data.Set as S

import Abstract
import Table
import Field
import Tuple

type AliasError = String 
type TypeError = String

-- takes map (Alias -> Table) and returns map (FieldName -> Alias)
buildFieldToAliasMap :: Map String Table -> Map String [String]
buildFieldToAliasMap aMap = 
    Prelude.foldl 
    (\ resMap (alias, table) -> 
        Prelude.foldl 
        (\ innerMap x -> M.insertWith (++) x [alias] innerMap) 
        resMap 
        (getFieldNames table)) 
    M.empty
    (M.toList aMap)

bindAliases :: Map String Table -> Predicate -> Either AliasError Predicate
bindAliases aMap = 
    aliasPredExpr (buildFieldToAliasMap aMap)

aliasPredExpr :: Map String [String] -> PredExpr a -> Either AliasError (PredExpr a)
aliasPredExpr f2aMap Always = Right Always
aliasPredExpr f2aMap (Eq e1 e2) = 
    case (aliasPredExpr f2aMap e1, aliasPredExpr f2aMap e2) of
        (Left error, _) -> Left error 
        (_, Left error) -> Left error
        (Right e1, Right e2) -> Right $ Eq e1 e2
aliasPredExpr f2aMap (Gt e1 e2) = 
    case (aliasPredExpr f2aMap e1, aliasPredExpr f2aMap e2) of
        (Left error, _) -> Left error 
        (_, Left error) -> Left error
        (Right e1, Right e2) -> Right $ Gt e1 e2
aliasPredExpr f2aMap (Lt e1 e2) = 
    case (aliasPredExpr f2aMap e1, aliasPredExpr f2aMap e2) of
        (Left error, _) -> Left error 
        (_, Left error) -> Left error
        (Right e1, Right e2) -> Right $ Lt e1 e2
aliasPredExpr f2aMap (And e1 e2) = 
    case (aliasPredExpr f2aMap e1, aliasPredExpr f2aMap e2) of
        (Left error, _) -> Left error 
        (_, Left error) -> Left error
        (Right e1, Right e2) -> Right $ And e1 e2
aliasPredExpr f2aMap (Or e1 e2) = 
    case (aliasPredExpr f2aMap e1, aliasPredExpr f2aMap e2) of
        (Left error, _) -> Left error 
        (_, Left error) -> Left error
        (Right e1, Right e2) -> Right $ Or e1 e2
aliasPredExpr f2aMap (Not e) = 
    case aliasPredExpr f2aMap e of
        Left error -> Left error
        Right e -> Right $ Not e
aliasPredExpr f2aMap (Add e1 e2) = 
    case (aliasPredExpr f2aMap e1, aliasPredExpr f2aMap e2) of
        (Left error, _) -> Left error 
        (_, Left error) -> Left error
        (Right e1, Right e2) -> Right $ Add e1 e2
aliasPredExpr f2aMap (Sub e1 e2) = 
    case (aliasPredExpr f2aMap e1, aliasPredExpr f2aMap e2) of
        (Left error, _) -> Left error 
        (_, Left error) -> Left error
        (Right e1, Right e2) -> Right $ Sub e1 e2
aliasPredExpr f2aMap (Mul e1 e2) = 
    case (aliasPredExpr f2aMap e1, aliasPredExpr f2aMap e2) of
        (Left error, _) -> Left error 
        (_, Left error) -> Left error
        (Right e1, Right e2) -> Right $ Mul e1 e2
aliasPredExpr f2aMap (Div e1 e2) = 
    case (aliasPredExpr f2aMap e1, aliasPredExpr f2aMap e2) of
        (Left error, _) -> Left error 
        (_, Left error) -> Left error
        (Right e1, Right e2) -> Right $ Div e1 e2
aliasPredExpr f2aMap (Mod e1 e2) = 
    case (aliasPredExpr f2aMap e1, aliasPredExpr f2aMap e2) of
        (Left error, _) -> Left error 
        (_, Left error) -> Left error
        (Right e1, Right e2) -> Right $ Mod e1 e2
aliasPredExpr f2aMap (Like e1 e2) = 
    case (aliasPredExpr f2aMap e1, aliasPredExpr f2aMap e2) of
        (Left error, _) -> Left error 
        (_, Left error) -> Left error
        (Right e1, Right e2) -> Right $ Like e1 e2
aliasPredExpr f2aMap (NumLit x) = 
    Right $ NumLit x
aliasPredExpr f2aMap (StrLit s) = 
    Right $ StrLit s
-- Only interesting part
aliasPredExpr f2aMap v@(Var (Explicit alias fName)) = 
    case M.lookup fName f2aMap of
        Nothing -> Left $ "No registered alias containing " ++ show fName
        Just xs -> 
            if alias `elem` xs then
                Right v -- of all possible aliases, `alias` was chosen 
            else
                Left $ "Alias " ++ show alias ++ " doesn't contain " ++ show fName ++ ".Possible aliases: " ++ intercalate ", " xs
aliasPredExpr f2aMap (Var (Implicit fName)) = 
    case M.lookup fName f2aMap of
        Nothing      -> Left $ "No registered alias containing " ++ show fName
        Just [alias] -> Right $ Var $ Explicit alias fName
        Just []      -> Left $ "No registered alias containing " ++ show fName
        Just xs      -> Left $ "Cannot deduce single alias for " ++ show fName ++ ".Possible aliases: " ++ intercalate ", " xs

toNNF :: PredExpr Bool -> PredExpr Bool
toNNF (Not (Not e)) = toNNF e
toNNF (Not (Or e1 e2)) =  And (toNNF $ Not e1) (toNNF $ Not e2)   
toNNF (Not (And e1 e2)) = Or (toNNF $ Not e1) (toNNF $ Not e2)
toNNF (And e1 e2) = And (toNNF e1) (toNNF e2)
toNNF (Or e1 e2) = Or (toNNF e1) (toNNF e2)
toNNF e = e

toCNF :: PredExpr Bool -> PredExpr Bool
toCNF (e1 `Or` e2) = case (toCNF e1, toCNF e2) of
    (e11 `And` e12, e21 `And` e22) -> (toCNF e11 `Or` toCNF e21) `And` ((toCNF e11 `Or` toCNF e22) `And` ((toCNF e12 `Or` toCNF e21) `And` (toCNF e12 `Or` toCNF e22)))
    (e11 `And` e12, e2)            -> (e11 `Or` e2) `And` (e12 `Or` e2)
    (           e1, e21 `And` e22) -> (e1 `Or` e21) `And` (e1 `Or` e22)
    (           e1, e2)            -> e1 `Or` e2
toCNF (e1 `And` e2) = toCNF e1 `And` toCNF e2
toCNF e = e

isCNF :: PredExpr Bool -> Bool
isCNF (e1 `Or` e2) = 
    (not (hasAnd e1) && not (hasAnd e2)) && isCNF e1 && isCNF e2 
isCNF (e1 `And` e2) = 
    isCNF e1 && isCNF e2
isCNF _ = True

hasAnd :: PredExpr Bool -> Bool
hasAnd (e1 `Or` e2) = hasAnd e1 || hasAnd e2
hasAnd (_ `And` _) = True
hasAnd _ = False

toCNFloop :: PredExpr Bool -> PredExpr Bool
toCNFloop e = 
    if isCNF e then
        e
    else
        toCNFloop $ toCNF e

-- Given a CNF predicate and list of aliases returns pair of CNF predicates
-- Left one contains only variables aliased by the ones from the list and the second one contains the rest  
separate :: Predicate -> [String] -> (Predicate, Predicate)
separate (e1 `And` e2) aliases = case (separate e1 aliases, separate e2 aliases) of
    ((Always, Always), (res2, left2))    -> (res2, left2)
    ((res1, left1)   , (Always, Always)) -> (res1, left1)
    ((Always, left1) , (Always, left2))  -> (Always, left1 `And` left2)
    ((res1, Always)  , (res2, Always))   -> (res1 `And` res2, Always)
    ((Always, left1) , (res2, left2))    -> (res2, left1 `And` left2)
    ((res1, Always)  , (res2, left2))    -> (res1 `And` res2, left2)
    ((res1, left1)   , (Always, left2))  -> (res1, left1 `And` left2)
    ((res1, left1)   , (res2, Always))   -> (res1 `And` res2, left1)
    ((res1, left1)   , (res2  , left2))  -> (res1 `And` res2, left1 `And` left2)
separate Always _ = (Always, Always)
separate e aliases = 
    if aliasesOcurringIn e `S.isSubsetOf` S.fromList aliases then
        (e, Always)
    else
        (Always, e)

aliasesOcurringIn :: PredExpr a -> Set String
aliasesOcurringIn (Var (Explicit alias _)) = 
    S.singleton alias
aliasesOcurringIn (e1 `And` e2) = 
    aliasesOcurringIn e1 `S.union` aliasesOcurringIn e2
aliasesOcurringIn (e1 `Or` e2) = 
    aliasesOcurringIn e1 `S.union` aliasesOcurringIn e2
aliasesOcurringIn (Not e) = 
    aliasesOcurringIn e
aliasesOcurringIn (e1 `Eq` e2) = 
    aliasesOcurringIn e1 `S.union` aliasesOcurringIn e2
aliasesOcurringIn (e1 `Gt` e2) = 
    aliasesOcurringIn e1 `S.union` aliasesOcurringIn e2
aliasesOcurringIn (e1 `Lt` e2) = 
    aliasesOcurringIn e1 `S.union` aliasesOcurringIn e2
aliasesOcurringIn (e1 `Add` e2) = 
    aliasesOcurringIn e1 `S.union` aliasesOcurringIn e2
aliasesOcurringIn (e1 `Sub` e2) = 
    aliasesOcurringIn e1 `S.union` aliasesOcurringIn e2
aliasesOcurringIn (e1 `Mul` e2) = 
    aliasesOcurringIn e1 `S.union` aliasesOcurringIn e2
aliasesOcurringIn (e1 `Div` e2) = 
    aliasesOcurringIn e1 `S.union` aliasesOcurringIn e2
aliasesOcurringIn (e1 `Mod` e2) = 
    aliasesOcurringIn e1 `S.union` aliasesOcurringIn e2
aliasesOcurringIn (e1 `Like` e2) = 
    aliasesOcurringIn e1 `S.union` aliasesOcurringIn e2
aliasesOcurringIn _ = S.empty

typeCheck :: Map (String, String) FieldType -> Predicate -> Maybe TypeError
typeCheck typeMap (e1 `And` e2) = boolOp typeMap e1 e2
typeCheck typeMap (e1 `Or` e2) = boolOp typeMap e1 e2
typeCheck typeMap (Not e) = typeCheck typeMap e
typeCheck typeMap (e1 `Eq` e2) = valOp typeMap e1 e2
typeCheck typeMap (e1 `Gt` e2) = valOp typeMap e1 e2
typeCheck typeMap (e1 `Lt` e2) = valOp typeMap e1 e2
typeCheck typeMap (e1 `Like` e2) = case binop typeMap e1 e2 (StringT 0) of
    Left err -> Just err
    Right _ -> Nothing
typeCheck typeMap Always = Nothing
typeCheck typeMap (Var _) = Just "Type error: No variable should be on this level of expression"
 
boolOp typeMap e1 e2 = 
    case (typeCheck typeMap e1, typeCheck typeMap e2) of 
        (Nothing, Nothing) -> Nothing
        (Just err, _) -> Just err
        (_, Just err) -> Just err
valOp typeMap e1 e2 = case (typeCheckAndGet typeMap e1, typeCheckAndGet typeMap e2) of
    (Left err, _) -> Just err
    (_, Left err) -> Just err
    (Right IntT, Right IntT) -> Nothing
    (Right (StringT _), Right (StringT _)) -> Nothing
    (Right x, Right y) -> Just $ "Type error: expecting " ++ show x ++ " but got " ++ show y

typeCheckAndGet :: Map (String, String) FieldType -> PredExpr a -> Either TypeError FieldType
typeCheckAndGet typeMap (e1 `Add` e2) = binop typeMap e1 e2 IntT
typeCheckAndGet typeMap (e1 `Sub` e2) = binop typeMap e1 e2 IntT
typeCheckAndGet typeMap (e1 `Mul` e2) = binop typeMap e1 e2 IntT
typeCheckAndGet typeMap (e1 `Div` e2) = binop typeMap e1 e2 IntT
typeCheckAndGet typeMap (e1 `Mod` e2) = binop typeMap e1 e2 IntT
typeCheckAndGet typeMap (e1 `Like` e2) = binop typeMap e1 e2 (StringT 0)
typeCheckAndGet typeMap (NumLit _) = Right IntT
typeCheckAndGet typeMap (StrLit _) = Right $ StringT 0
typeCheckAndGet typeMap (Var (Explicit alias fName)) = 
    case M.lookup (alias, fName) typeMap of
        Nothing -> Left $ "Type error: cannot deduce type for " ++ show (alias ++ "." ++ fName) 
        Just t -> Right t
typeCheckAndGet _ _ = Left $ ""

binop :: Map (String, String) FieldType -> PredExpr a1 -> PredExpr a2 -> p -> Either TypeError FieldType
binop typeMap e1 e2 exT = case (typeCheckAndGet typeMap e1, typeCheckAndGet typeMap e2) of 
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right IntT, Right IntT) -> Right IntT
    (Right (StringT _), Right (StringT _)) -> Right $ StringT 0
    (Right x, Right y) -> Left $ "Type error: expecting" ++ show x ++ " but got " ++ show y

evalPred :: M.Map (String, String) Field -> Predicate -> Either EvalError Bool
evalPred _ Always = Right True
evalPred vMap (e1 `And` e2) = case (evalPred vMap e1, evalPred vMap e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right v1, Right v2) -> Right $ v1 && v2
evalPred vMap (e1 `Or` e2) = case (evalPred vMap e1, evalPred vMap e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right v1, Right v2) -> Right $ v1 || v2
evalPred vMap (Not e) = case evalPred vMap e of
    Left err -> Left err
    Right v -> Right $ not v
evalPred vMap (e1 `Eq` e2) = case (eval vMap e1, eval vMap e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right v1, Right v2) -> Right $ v1 == v2
evalPred vMap (e1 `Gt` e2) = case (eval vMap e1, eval vMap e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right v1, Right v2) -> Right $ v1 > v2
evalPred vMap (e1 `Lt` e2) = case (eval vMap e1, eval vMap e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right v1, Right v2) -> Right $ v1 < v2
evalPred vMap (e1 `Like` e2) = case (eval vMap e1, eval vMap e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right v1, Right v2) -> Right $ v1 `isInfixOf` v2 -- not exactly like
evalPred vMap v@(Var _) = Left $ "Forbidden boolean variable " ++ show v -- impossible




instance Evaluable PredExpr String where
    eval = evalStringExpr

instance Evaluable PredExpr Int where
    eval = evalIntExpr


evalIntExpr :: M.Map (String, String) Field -> PredExpr Int -> Either EvalError Int
evalIntExpr vMap (e1 `Add` e2) = case (evalIntExpr vMap e1, evalIntExpr vMap e2) of 
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right v1, Right v2) -> Right $ v1 + v2
evalIntExpr vMap (e1 `Sub` e2) = case (evalIntExpr vMap e1, evalIntExpr vMap e2) of 
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right v1, Right v2) -> Right $ v1 - v2
evalIntExpr vMap (e1 `Mul` e2) = case (evalIntExpr vMap e1, evalIntExpr vMap e2) of 
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right v1, Right v2) -> Right $ v1 * v2
evalIntExpr vMap (e1 `Div` e2) = case (evalIntExpr vMap e1, evalIntExpr vMap e2) of 
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right v1, Right v2) -> Right $ v1 `div` v2
evalIntExpr vMap (e1 `Mod` e2) = case (evalIntExpr vMap e1, evalIntExpr vMap e2) of 
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right v1, Right v2) -> Right $ v1 `mod` v2
evalIntExpr vMap (NumLit x) = Right x
evalIntExpr vMap (Var (Explicit alias fName)) = 
    case M.lookup (alias, fName) vMap of
        Just (IntF x) -> Right x
        Just x -> Left $ "Unfound type error in " ++ show (alias ++ "." ++ fName) ++ "Expecting int, but got " ++ show x
        Nothing -> Left $ "Unbound variable " ++ show (alias ++ "." ++ fName)
evalIntExpr vMap (Var (Implicit fName)) = 
    Left $ "Unaliased variable " ++ show fName

evalStringExpr :: M.Map (String, String) Field -> PredExpr String -> Either EvalError String
evalStringExpr vMap (StrLit s) = Right s
evalStringExpr vMap (Var (Explicit alias fName)) = 
    case M.lookup (alias, fName) vMap of
        Just (StringF s) -> Right s
        Just x -> Left $ "Unfound type error in " ++ show (alias ++ "." ++ fName) ++" Expecting string, but got " ++ show x
        Nothing -> Left $ "Unbound variable " ++ show (alias ++ "." ++ fName)
evalStringExpr vMap (Var (Implicit fName)) = 
    Left $ "Unaliased variable " ++ show fName
