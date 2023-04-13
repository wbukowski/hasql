{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module File where

import System.IO
    ( Handle, openFile, IOMode(ReadMode, WriteMode, AppendMode), withBinaryFile, hSeek, SeekMode (AbsoluteSeek, RelativeSeek), hGetPosn, hIsSeekable )
import Data.Binary (decode, encode, Binary, decodeOrFail)
import Data.ByteString.Lazy ( hGetContents, hPut, ByteString, take )
import Data.ByteString.Lazy as B
import Data.Bits
import GHC.Int (Int64)
import qualified Data.List
import GHC.Generics (Generic)
import Data.Binary.Get (ByteOffset)

import Field
import DbStream
import Tuple
import Page
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)
import System.Posix (fileExist)
import Abstract (FieldSpecifier)


data DbFile =
    File [FieldType] FilePath
    deriving (Show, Generic, Binary)

getPath :: DbFile -> FilePath
getPath (File _ path) = path

readPages :: DbFile -> IO [Page]
readPages (File scheme path) = do
    withBinaryFile path ReadMode (\h -> do
        bs <- hGetContents h
        if B.length bs `mod` pagesize /= 0 then
            fail "File corrupted"
        else do
            return $ decodePages bs 0
        )
    where
        tupSize = getTupleSize scheme
        decodePages bs index = case decodePageContent bs of
            (page, left) -> let result = Page index tupSize page in 
                if left == B.empty then
                    [result]
                else
                    result : decodePages left (index+1) 

writePages :: DbFile -> [Page] -> IO ()
writePages (File scheme path) pages = do
    withBinaryFile path WriteMode (\h -> do
        hPut h $ B.concat $ Data.List.map encodePage pages)

readPage :: DbFile -> Int -> IO Page
readPage (File scheme path) no = do
    withBinaryFile path ReadMode (\h -> do
        hSeek h AbsoluteSeek (fromIntegral no * fromIntegral pagesize)
        bs <- hGet h (fromIntegral pagesize)
        return $ Page no (getTupleSize scheme) bs)

writePage :: DbFile -> Page -> IO ()
writePage (File scheme path) page@(Page no _ bs) = do
    withBinaryFile path WriteMode (\h -> do
        if B.length bs `mod` pagesize /= 0 then
            fail "Data page got corrupted"
        else do
        hSeek h AbsoluteSeek (fromIntegral no * fromIntegral pagesize)
        hPut h (encodePage page))

pageToTuples :: DbFile -> Page -> [FieldSpecifier] -> [Tuple]
pageToTuples (File scheme _) page@(Page no tupSize content) fNames = 
    decodeTuples scheme (getData page) (no * fromIntegral (getTPP tupSize))  
    where 
    decodeTuples scheme bs index 
        | bs == B.empty = []
        | isNthSlotFree (getHeader page) index = decodeTuples scheme (B.drop tupSize bs) index
        | otherwise = case decodeTuple bs scheme of
            (fields, left) -> let tuple = Tuple index scheme fNames fields in
                tuple : decodeTuples scheme left (index+1)

seqScan :: DbFile -> [FieldSpecifier] -> IO (DbStream i Tuple Int)
seqScan file@(File scheme path) fNames = do
    pages <- readPages file
    let tuples = Prelude.foldr (\ page tups -> pageToTuples file page fNames ++ tups) [] pages
    return $ fromList tuples 0


insertTuple :: DbFile -> Tuple -> IO ()
insertTuple file@(File scheme path) t@(Tuple tupNo _ _ fields) = do
    createDirectoryIfMissing True $ takeDirectory path
    cond <- fileExist path
    pages <- 
        if cond then do
            readPages file
        else do
            return []
    writePage file $ insertToPage (freePage pages) t
    where
        freePage pages = case Data.List.find hasFreeSlots pages of 
            Just page -> page
            Nothing -> newPage (getTupleSize scheme) (Prelude.length pages) 
    
deleteTuple :: DbFile -> Tuple -> IO ()
deleteTuple file@(File scheme path) t = do
    page <- readPage file (getPageNo t)
    writePage file $ deleteFromPage page t
