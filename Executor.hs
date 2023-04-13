module Executor where

import Data.Map.Lazy (Map, insert, delete)
import System.Directory (removeFile)

import Concretizer
import Table
import File
import Concretizer
import DbStream
import Tuple
import Operator
import Page (getPageNo, getPageIndex)

execute :: String -> Map String Table -> ConcretizationResult -> IO (String, Map String Table)
execute output tMap (Left concretizationError) = do
    putStrLn "Error occured"
    fail concretizationError

execute output tMap (Right (Left (C t))) = 
    return (output ++ ", created table '" ++ getTableName t ++ "'", insert (getTableName t) t tMap)

execute output tMap (Right (Left (D t))) = do
    removeFile $ getPath $ getFile t
    return (output ++ ", dropped table '" ++ getTableName t ++ "'", delete (getTableName t) tMap)

execute output tMap (Right (Left (I tab tup))) = do
    Table.insertTuple tab tup
    return (output ++ ", inserted tuple " ++ show tup ++ " into table " ++ show (getTableName tab), tMap)

execute output tMap (Right (Left (U tab plan applyUpdate))) = do
    stream <- executePlan plan
    res <- deleteStream tab stream
    let updatedStream = streamMap applyUpdate stream
    res <- insertStream tab updatedStream
    return (output ++ ", updated " ++ show res ++ " tuples in table " ++ show (getTableName tab), tMap)

execute output tMap (Right (Left (Del tab plan))) = do
    stream <- executePlan plan
    res <- deleteStream tab stream
    return (output ++ ", deleted " ++ show res ++ " tuples from table " ++ show (getTableName tab), tMap)

execute output tMap (Right (Right (Q node))) = do
    stream <- executePlan node
    res <- outputStream stream
    return (output ++ ", performed query with " ++ show res ++ " results", tMap)

executePlan :: PlanNode -> IO (DbStream i Tuple Int)
executePlan (ReadTable t) = 
    readTable t
executePlan (Filter pred node) = do
    stream <- executePlan node
    return (stream |>| filterOp pred)
executePlan Output =
    return $ Return (-1)
executePlan (Join n1 n2) = do
    s1 <- executePlan n1
    s2 <- executePlan n2
    return (combine s1 s2)
executePlan (Projection n) = 
    executePlan n -- TODO
    
prettyTup :: Tuple -> String
prettyTup tup = show $ getFields tup 

outputStream :: DbStream i Tuple a -> IO a
outputStream (Write o s) = do
    putStrLn $ prettyTup o
    outputStream s
outputStream (Return x) =
    return x
outputStream (Read f) = 
    outputStream $ f Nothing

insertStream :: Table -> DbStream i Tuple a -> IO a
insertStream tab (Return x) = 
    return x
insertStream tab (Read f) = 
    insertStream tab $ f Nothing
insertStream tab (Write o s) = do
    Table.insertTuple tab o
    insertStream tab s

deleteStream :: Table -> DbStream i Tuple a -> IO a
deleteStream tab (Return x) = 
    return x
deleteStream tab (Read f) =
    deleteStream tab $ f Nothing
deleteStream tab (Write o s) = do
    Table.deleteTuple tab o
    deleteStream tab s