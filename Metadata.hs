

module Metadata where


import Data.Binary (encode, decode)
import Data.Map.Lazy (Map, fromList, toList)
import Data.ByteString.Lazy as B
-- import Data.ByteString.Internal as B

import Table
import System.IO ( openFile, IOMode(ReadMode, AppendMode, WriteMode), hClose, hIsClosed, hSetBinaryMode, withBinaryFile )
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.Posix (fileExist)

tablesFile :: String
tablesFile = "./metadata/tables"

readTablesInfo :: IO [Table]
readTablesInfo = do
    createDirectoryIfMissing True $ takeDirectory tablesFile
    cond <- fileExist tablesFile
    if not cond then 
        return []
    else do
    withBinaryFile tablesFile ReadMode (\ h -> do
        bs <- hGetContents h
        mapM return (decode bs)) -- "forcing" evaluation

putTablesInfo :: [Table] -> IO ()
putTablesInfo tables = 
    withBinaryFile tablesFile WriteMode (\h -> do
        hPut h $ encode tables)

getTablesMap :: IO (Map String Table)
getTablesMap = do
    tables <- readTablesInfo
    let entries = Prelude.zip (Prelude.map (\ (Table name _ _ _) -> name) tables) tables 
    return $ fromList entries
        
putTablesMap :: Map String Table -> IO ()
putTablesMap tMap = do
    let tables = Prelude.map snd $ toList tMap
    putTablesInfo tables