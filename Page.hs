{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Page where

import GHC.Int (Int64, Int32)
import Data.ByteString.Lazy as B
    ( ByteString, append, concat, drop, empty, length, splitAt, take )
import Data.Binary (Binary, encode, decode, Word64, decodeOrFail)
import Data.List ( find )
import Data.Bits ( Bits(clearBit, testBit, setBit) )


import Field ( Field, FieldType(..) )
import Tuple ( Tuple(..), decodeTuple, encodeTuple, expand )


pagesize = 4096


type TupleSize = Int64

data Header =
    Header [Int32] Int64
    --     content len
    deriving Show

type PageContent = ByteString

data Page = 
    Page Int Int64 PageContent
    -- pageNo tupSize pageContent
    deriving (Show)


encodePage :: Page -> ByteString
encodePage (Page _ _ content) = expand content pagesize
decodePageContent = B.splitAt pagesize

getTupleSize :: [FieldType] -> TupleSize
getTupleSize = 
    Prelude.foldr foo 0
    where
        foo IntT = (+) $ B.length $ encode (0 :: Int)
        foo (StringT len)= (+) $ B.length $ encode (Prelude.replicate len '_')

getTPP :: TupleSize -> Int64
getTPP tupSize = floor $ fromIntegral (pagesize * 8) / (fromIntegral tupSize * 8 + 1)

getHeaderSize :: TupleSize -> Int64
getHeaderSize tupSize =  lcm 4 (ceiling $ fromIntegral (getTPP tupSize) / 8)

getHeaderLen :: TupleSize -> Int64
getHeaderLen tupSize = fromIntegral (getHeaderSize tupSize) `div` 4

getHeader :: Page -> Header
getHeader (Page _ tupSize content) =
    let headerSize = getHeaderSize tupSize in
    Header (decodeIntList content (getHeaderLen tupSize)) (getTPP tupSize)
    -- where 
        
decodeIntList :: ByteString -> Int64 -> [Int32]
decodeIntList bs 0 = []
decodeIntList bs toDecode = 
    case decodeOrFail bs of
        Left _ -> []
        Right (left, _, res) -> res : decodeIntList left (toDecode - 1)

insertHeader :: Page -> Header -> Page
insertHeader page@(Page no ts _) (Header h len) =
    Page no ts $ B.append (encodeIntList h) (getData page)
    -- where 
encodeIntList :: [Int32] -> ByteString
encodeIntList = Prelude.foldr (B.append . encode) B.empty

getHeaderInternal :: Header -> [Int32]
getHeaderInternal (Header con _) = con

getPageNo :: Tuple -> Int
getPageNo (Tuple no scheme _ _) = 
    no `div` fromIntegral (getTupleSize scheme)

getPageIndex :: Tuple -> Int
getPageIndex t@(Tuple no scheme _ _) = 
    no - (getPageNo t * fromIntegral (getTPP (getTupleSize scheme)))


getData :: Page -> ByteString
getData (Page _ tupSize p) = 
    B.drop (getHeaderSize tupSize) p

getContent :: Page -> ByteString
getContent (Page _ _ content) = content

insertData :: Page -> ByteString -> Page
insertData (Page no tupSize p) d = 
    Page no tupSize $ B.append (B.take (getHeaderSize tupSize) p) d

hasFreeSlots :: Page -> Bool
hasFreeSlots p = 
    case firstFreeSlot (getHeader p) of
        Just _  -> True
        Nothing -> False

firstFreeSlot :: Header -> Maybe Int
firstFreeSlot header@(Header h tupSize) =
    Data.List.find (isNthSlotFree header) [0..(fromIntegral $ getTPP tupSize)]

isNthSlotFree :: Header -> Int -> Bool
isNthSlotFree (Header h len) n =
    not $ getBit h n

markUsed :: Header -> Int -> Header
markUsed (Header h len) index = 
    Header (mark h index True) len

markFree :: Header -> Int -> Header
markFree (Header h len) index = 
    Header (mark h index False) len

getBit :: [Int32] -> Int -> Bool
getBit [] _ = True
getBit (x : xs) index = 
    if index < 32 then
        testBit x index
    else 
        getBit xs (index-32) 

mark :: [Int32] -> Int -> Bool -> [Int32]
mark [] _ _ = []
mark (x : xs) index bit = 
    if index < 32 then
        if bit then
            setBit x index : xs 
        else 
            clearBit x index : xs
    else 
        x : mark xs (index-32) bit

insertAtNthSlot :: Page -> Tuple -> Int -> Page
insertAtNthSlot page@(Page no tupSize content) t index =
    let new_header = markUsed (getHeader page) index in
    insertData (insertHeader page new_header) new_data
    where
        n :: Int64
        n = fromIntegral index
        old_data = getData page
        new_data = B.concat [B.take (tupSize*n) old_data, encodeTuple t, B.drop (tupSize*(n+1)) old_data]

getNthTuple :: Page -> Int -> Maybe ([Field], ByteString)
getNthTuple page@(Page _ tupSize _) n = 
    if isNthSlotFree (getHeader page) n then
        Nothing
    else
        Just $ decodeTuple (B.take tupSize $ B.drop (tupSize * fromIntegral n) (getData page)) [StringT 30, IntT]

insertToPage :: Page -> Tuple -> Page
insertToPage p t = 
    case firstFreeSlot (getHeader p) of
        Just n -> insertAtNthSlot p t n
        Nothing -> p

deleteFromPage :: Page -> Tuple -> Page
deleteFromPage p t = 
    let new_header = markFree (getHeader p) (getPageIndex t) in
    insertHeader p new_header


newPage :: TupleSize -> Int -> Page
newPage tupSize no = 
    Page no tupSize (expand B.empty pagesize)
