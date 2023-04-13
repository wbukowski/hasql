{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Table where

import DbStream
import Tuple
import Text.Read
import Field
import File

import GHC.Generics
import Data.Binary
import GHC.IO.IOMode (IOMode(WriteMode, AppendMode))
import Data.ByteString.Lazy (length)
import GHC.Int (Int64)
import Abstract (FieldSpecifier(Explicit))



data Table = Table String DbFile [String] [FieldType]
    deriving (Show, Generic, Binary)

getFile :: Table -> DbFile
getFile (Table _ file  _ _) = file

getFieldNames :: Table -> [String]
getFieldNames (Table _ _ fNames _) = 
    fNames

getScheme :: Table -> [FieldType]
getScheme (Table _ _ _ scheme) = scheme

getTableName :: Table -> String
getTableName (Table name _ _ _) = name

readTable :: Table -> IO (DbStream i Tuple Int)
readTable t = 
    seqScan (getFile t) $ Prelude.map (Explicit (getTableName t)) (getFieldNames t)

insertTuple :: Table -> Tuple -> IO ()
insertTuple (Table _ file _ _) = 
    File.insertTuple file

deleteTuple :: Table -> Tuple -> IO ()
deleteTuple (Table _ file _ _) = 
    File.deleteTuple file


