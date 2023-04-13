{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Abstract where 

import qualified Data.Map as Data.Map.Lazy

import Field (Field)
    
newtype Program = Program [Statement]

data Statement
    = TableCommand TableCommand
    | Query Query
    deriving (Show)

data TableCommand 
    = CREATE String [ColumnInfo]
    -- CREATE TABLE *tablename* (column1 datatype1, column2 datatype2, ...)
    | DROP String
    -- DROP TABLE *tablename*
    | INSERT String [String] [Data]
    -- INSERT INTO *tablename* (column1, column2, ...) VALUES (value1, value2, ...)
    | UPDATE String [(String, Data)] Predicate
    -- UPDATE *tablename* SET column1 = value1, column2 = value2, ... WHERE *predicate*
    | DELETE String Predicate
    -- DELETE FROM *tablename* WHERE *predicate*
    deriving (Show)

data ColumnInfo
    = ColumnInfo String DataType
    deriving (Show)

data Query
    = SELECT ColumnSelector [TableName] Predicate ColumnSelector
--    SELECT *columns* FROM *tablename* WHERE *predicate* GROUP BY *columns*
    deriving (Show)

data ColumnSelector
    = Specific [FieldSpecifier]
    | All --  *
    deriving (Show)

type Predicate = PredExpr Bool

data PredExpr a where
    Always :: PredExpr Bool
    Eq  :: (Eq a, Evaluable PredExpr a, Show a)  => PredExpr a -> PredExpr a -> PredExpr Bool
    Gt  :: (Ord a, Evaluable PredExpr a)  => PredExpr a -> PredExpr a -> PredExpr Bool
    Lt  :: (Ord a, Evaluable PredExpr a)  => PredExpr a -> PredExpr a -> PredExpr Bool

    And :: PredExpr Bool -> PredExpr Bool -> PredExpr Bool 
    Or  :: PredExpr Bool -> PredExpr Bool -> PredExpr Bool
    Not :: PredExpr Bool -> PredExpr Bool

    Add :: PredExpr Int -> PredExpr Int -> PredExpr Int
    Sub :: PredExpr Int -> PredExpr Int -> PredExpr Int
    Mul :: PredExpr Int -> PredExpr Int -> PredExpr Int
    Div :: PredExpr Int -> PredExpr Int -> PredExpr Int
    Mod :: PredExpr Int -> PredExpr Int -> PredExpr Int

    Like :: PredExpr String -> PredExpr String -> PredExpr Bool 

    Var :: FieldSpecifier -> PredExpr a
    NumLit :: Int -> PredExpr Int
    StrLit :: String -> PredExpr String

instance Show (PredExpr a) where
    show Always = "Always"
    show (Eq e1 e2) = addParens $ "Eq " ++ show e1 ++ ", " ++ show e2
    show (Gt e1 e2) = addParens $ "Gt " ++ show e1 ++ ", " ++ show e2
    show (Lt e1 e2) = addParens $ "Lt " ++ show e1 ++ ", " ++ show e2

    show (And e1 e2) = addParens $ "And " ++ show e1 ++ ", " ++ show e2
    show (Or e1 e2) = addParens $ "Or " ++ show e1 ++ ", " ++ show e2
    show (Not e) = addParens $ "Not " ++ show e

    show (Add e1 e2) = addParens $ "Add" ++ show e1 ++ ", " ++ show e2
    show (Sub e1 e2) = addParens $ "Sub" ++ show e1 ++ ", " ++ show e2
    show (Mul e1 e2) = addParens $ "Mul" ++ show e1 ++ ", " ++ show e2
    show (Div e1 e2) = addParens $ "Div" ++ show e1 ++ ", " ++ show e2
    show (Mod e1 e2) = addParens $ "Mod" ++ show e1 ++ ", " ++ show e2

    show (Like e1 e2) = addParens $ "Mod" ++ show e1 ++ ", " ++ show e2

    show (Var fs) = addParens $ "Var " ++ show fs
    show (NumLit fs) = addParens $ "NumLit " ++ show fs
    show (StrLit fs) = addParens $ "StrLit " ++ show fs

addParens :: String -> String
addParens s = "(" ++ s ++ ")"

data PredOp
    = EQ    -- =
    | NEQ   -- != || <>
    | GT    -- >
    | LT    -- <
    | GTE   -- >=
    | LTE   -- <=
    deriving (Show)

data DataType
    = StringType Int
    | IntType
    deriving (Show)

type EvalError = String

class Evaluable f a where
    eval :: Data.Map.Lazy.Map (String, String) Field -> f a -> Either EvalError a

data Data
    = String String
    | Int Int
    deriving (Show)

data TableName 
    = Aliased String String
    | Single String
    deriving (Show)

data FieldSpecifier 
    = Explicit String String
    | Implicit String
--  | Aggregate -- Possible future feature
    deriving (Show)
