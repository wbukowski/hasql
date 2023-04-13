{-# LANGUAGE FlexibleContexts #-}

module SQLParser where

import Pred
import Abstract
import Text.ParserCombinators.Parsec
import Control.Monad (void, guard)
import qualified Abstract
import Text.ParserCombinators.Parsec.Expr (buildExpressionParser, Operator (Infix, Prefix), Assoc (AssocLeft))

-- Additional functions

-- Give parser an ability to parse trailing whitespace
lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

keyword :: String -> Parser String
keyword s = try $ lexeme $ do
    r <- string s
    guard (r == s)
    return s

operator :: Parser String
operator = try $ lexeme $ 
    many1 $ oneOf "<>=!+-*/%"

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

parens :: Parser a -> Parser a
parens = between (keyword "(") (keyword ")")

optionalParens :: Parser a -> Parser a
optionalParens p = try (parens p) <|> p

num :: Parser Int
num = do    
    n <- lexeme $ many1 digit
    return (read n)

-- Parses any string starting with a letter or an underscore, containing only letter, digits or underscores
str :: Parser String
str = lexeme $ do
    f <- firstChar
    rest <- many nonFirstChar
    return $ f : rest
    where
        firstChar = letter <|> char '_'
        nonFirstChar = digit <|> firstChar

symbol :: String -> Parser String
symbol s = try $ lexeme $ do
    r <- operator
    guard (s == r)
    return r

strLiteral :: Parser String
strLiteral = lexeme $ do
    between (keyword "'") (keyword "'") $ many $ satisfy (/= '\'')

dataType :: Parser DataType
dataType = lexeme $ 
    intType <|> stringType
    where
        intType = do
            keyword "INT"
            return IntType
        stringType = do
            keyword "STRING"
            StringType <$> num

dataValue :: Parser Data
dataValue = lexeme $ 
    Int <$> num <|> String <$> strLiteral

columnInfo :: Parser ColumnInfo
columnInfo = do
    fieldName <- str
    fieldType <- dataType
    return $ ColumnInfo fieldName fieldType

tableName :: Parser TableName
tableName = 
    try aliasedName <|> singleName
    where
        aliasedName = do
            alias <- str
            keyword "AS"
            name <- str
            return $ Aliased alias name
        singleName = do
            Single <$> str

fieldSpecifier :: Parser FieldSpecifier
fieldSpecifier = 
    try explicitSpecifier <|> implicitSpecifier
    where 
        explicitSpecifier = do
            alias <- str
            keyword "."
            field <- str
            return $ Explicit alias field
        implicitSpecifier = 
            Implicit <$> str

-- Table commands parsing

tableCommand :: Parser TableCommand
tableCommand = lexeme $ 
    createCommand <|> dropCommand <|> insertCommand <|> updateCommand <|> deleteCommand

createCommand :: Parser TableCommand
createCommand = do
    keyword "CREATE TABLE"
    tableName <- str
    fields <- optionalParens $ sepBy1 columnInfo (lexeme $ char ',')
    return $ CREATE tableName fields

dropCommand :: Parser TableCommand
dropCommand = lexeme $ do
    keyword "DROP TABLE"
    DROP <$> str

insertCommand :: Parser TableCommand
insertCommand = lexeme $ do
    keyword "INSERT INTO"
    tableName <- str
    columns <- try (parens $ sepBy str (lexeme $ char ',')) <|> return []
    keyword "VALUES"
    values <- parens $ sepBy1 dataValue (lexeme $ char ',')
    return $ INSERT tableName columns values

updateElement :: Parser (String, Data)
updateElement = lexeme $ do
    fieldName <- str
    lexeme $ char '='
    value <- dataValue
    return (fieldName, value)

updateCommand :: Parser TableCommand
updateCommand = lexeme $ do
    keyword "UPDATE"
    tableName <- str
    keyword "SET"
    elements <- optionalParens $ sepBy1 updateElement (lexeme $ char ',')
    keyword "WHERE"
    pred <- predicate
    return $ UPDATE tableName elements pred

deleteCommand :: Parser TableCommand
deleteCommand = lexeme $ do
    keyword "DELETE FROM"
    tableName <- str
    keyword "WHERE"
    pred <- predicate
    return $ DELETE tableName pred


-- Predicate parsing

predicate :: Parser Predicate
predicate = predExprBool

predExprBool :: Parser (PredExpr Bool)
predExprBool = buildExpressionParser table term
    where
        table = [[Prefix (Not <$ keyword "NOT")]
                ,[Infix (And <$ keyword "AND") AssocLeft]
                ,[Infix ( Or <$ keyword "OR" ) AssocLeft]]
        term = try parensBoolExpr <|> opBoolExpr

opBoolExpr = try opBoolNumExpr <|> opBoolStrExpr

parensBoolExpr :: Parser (PredExpr Bool)
parensBoolExpr = parens predExprBool <|> parens opBoolNumExpr <|> parens opBoolStrExpr

opBoolNumExpr :: Parser (PredExpr Bool)
opBoolNumExpr = do
    e1 <- opNumExpr
    op <- operator
    e2 <- opNumExpr
    case opToConstr op of 
        (Just c) -> return $ c e1 e2
        Nothing -> fail "Operator not supported"
    where 
        -- opToConstr :: Eq a => String -> Maybe (PredExpr a -> PredExpr a -> PredExpr Bool)
        opToConstr "=" = Just Abstract.Eq
        opToConstr "!=" = Just (\ e1 e2 -> Abstract.Not (Abstract.Eq e1 e2))
        opToConstr "<>" = Just (\ e1 e2 -> Abstract.Not (Abstract.Eq e1 e2))
        opToConstr ">" = Just Abstract.Gt
        opToConstr "<" = Just Abstract.Lt
        opToConstr ">=" = Just (\ e1 e2 -> Abstract.Not (Abstract.Lt e1 e2))
        opToConstr "<=" = Just (\ e1 e2 -> Abstract.Not (Abstract.Gt e1 e2))
        opToConstr _ = Nothing

opBoolStrExpr :: Parser (PredExpr Bool)
opBoolStrExpr = do
    e1 <- StrLit <$> strLiteral <|> varExpr
    op <- keyword "LIKE" <|> operator
    e2 <- StrLit <$> strLiteral <|> varExpr
    case opToConstr op of 
        Just c -> return $ c e1 e2
        Nothing -> fail "Operator not supported"
    where 
        opToConstr "=" = Just Abstract.Eq
        opToConstr "!=" = Just (\ e1 e2 -> Abstract.Not (Abstract.Eq e1 e2))
        opToConstr "<>" = Just (\ e1 e2 -> Abstract.Not (Abstract.Eq e1 e2))
        opToConstr ">" = Just Abstract.Gt
        opToConstr "<" = Just Abstract.Lt
        opToConstr ">=" = Just (\ e1 e2 -> Abstract.Not (Abstract.Lt e1 e2))
        opToConstr "<=" = Just (\ e1 e2 -> Abstract.Not (Abstract.Gt e1 e2))
        opToConstr "LIKE" = Just Abstract.Like
        opToConstr _ = Nothing

opNumExpr :: Parser (PredExpr Int)
opNumExpr = buildExpressionParser table term
    where 
        table = [[Infix (Mul <$ symbol "*") AssocLeft
             ,Infix (Div <$ symbol "/") AssocLeft]
            ,[Infix (Add <$ symbol "+") AssocLeft
             ,Infix (Sub <$ symbol "-") AssocLeft]]
        term  = try parensNumExpr <|> try numLitIntExpr <|> varExpr

parensNumExpr :: Parser (PredExpr Int)
parensNumExpr = parens opNumExpr <|> parens numLitIntExpr <|> parens varExpr

alwaysPred :: Parser (PredExpr Bool)
alwaysPred = return Always

varExpr :: Parser (PredExpr a)
varExpr = optionalParens $ 
    Var <$> fieldSpecifier

numLitIntExpr :: Parser (PredExpr Int)
numLitIntExpr =  
    NumLit <$> num

-- -- Query parsing

columnSelector :: Parser ColumnSelector
columnSelector = 
    starSelector <|> specificSelector
    where
        starSelector = do
            keyword "*"
            return All
        specificSelector = do
            fields <- sepBy1 fieldSpecifier (lexeme $ char ',')
            return $ Specific fields

allSelector :: Parser ColumnSelector
allSelector = return All

query :: Parser Query
query = lexeme $ do
    keyword "SELECT"
    colSel <- columnSelector
    keyword "FROM"
    names <- optionalParens $ sepBy1 tableName (lexeme $ char ',')
    pred <- (do 
            keyword "WHERE"
            predicate) 
            <|>
            alwaysPred 
    grBy <- (do
            keyword "GROUP BY"
            columnSelector)
            <|>
            allSelector
    return $ SELECT colSel names pred grBy

-- Program parsing

statement :: Parser Statement
statement = 
    TableCommand <$> tableCommand <|> Query <$> query

program :: Parser [Statement]
program = 
    sepEndBy statement (lexeme $ char ';') 


-- Parses whole string and returns an error if there is anything left
parseWithEOF :: Parser a -> String -> Either ParseError a
parseWithEOF p =
    parse (p <* eof) ""
