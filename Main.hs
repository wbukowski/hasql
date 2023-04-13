
import GHC.IO.Handle (hSetBuffering, BufferMode (NoBuffering), isEOF, hFlush)
import GHC.IO.Handle.FD (stdout, stdin)
import Data.Map.Lazy (Map)
import Control.Monad (foldM_, foldM)

import Metadata (getTablesMap, putTablesMap)
import SQLParser
import Table
import Concretizer
import Executor

-- loop :: IO ()
loop :: Map String Table -> IO (Map String Table)
loop tableMap = do
    putStr "λ> " 
    hFlush stdout
    done <- isEOF
    if done then 
        return tableMap
    else do
        input <- getLine 
        if input == "exit" then
            return tableMap
        else do
            let abstract = parseWithEOF program input 
            (res, newTableMap) <- case abstract of 
                Left parseError -> return (show parseError, tableMap)
                Right statements ->
                    foldM 
                    (\ (programRes, tMap) abstractStatement -> do
                        let concreteStatement = concretize tMap abstractStatement
                        execute programRes tMap concreteStatement)
                    ("", tableMap)
                    statements 
            putStrLn (drop 2 res) -- initial ',' and one space 
            loop newTableMap
            
            
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    tableMap <- getTablesMap
    finalMap <- loop tableMap
    putStrLn "Closing..."
    putTablesMap finalMap
    return ()