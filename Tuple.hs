{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Tuple where

import Field
import Data.Binary
import GHC.Generics (Generic)
import Data.ByteString.Lazy as B
import GHC.Int (Int64)
import Data.List as L
import Abstract (FieldSpecifier (Implicit, Explicit))

data Tuple = Tuple Int [FieldType] [FieldSpecifier] [Field]
             -- tupNo scheme fieldNames fields


class Mergable a where
    merge :: a -> a -> a
    
instance Mergable Tuple where
    merge = mergeTup

mergeTup :: Tuple -> Tuple -> Tuple 
mergeTup (Tuple _ scheme1 names1 fields1) (Tuple _ scheme2 names2 fields2) = 
    Tuple (-1) (scheme1 ++ scheme2) (names1 ++ names2) (fields1 ++ fields2)

instance Show Tuple where
    show (Tuple _ _ _ fields) = show fields

decodeTuple :: ByteString -> [FieldType] -> ([Field], ByteString)
decodeTuple bs fTs = 
    case Prelude.foldl foo ([], bs) fTs of
        (fields, left) -> (L.reverse fields, left)
    where
        foo (fs, bs) IntT = case decodeOrFail bs of
            Left _ -> (fs, bs)
            Right (left, _, x) -> (IntF x : fs, left)
        foo (fs, bs) (StringT len) = case decodeOrFail bs of
            Left _ -> ([], bs)
            Right (left, _, s) -> (StringF s : fs, B.drop (fromIntegral $ len - Prelude.length s) left)

encodeTuple :: Tuple -> ByteString
encodeTuple (Tuple _ scheme _ fields) = 
    B.concat $ Prelude.map encodeField $ Prelude.zip scheme fields
    where 
        encodeField :: (FieldType, Field) -> ByteString
        encodeField (IntT, IntF x) = encode x
        encodeField (StringT len, StringF s) = expand (encode s) (B.length $ encode (Prelude.replicate len '_'))
        encodeField (IntT, Null) = expand B.empty 8
        encodeField (StringT len, Null) = expand B.empty (B.length $ encode (Prelude.replicate len '_'))
        encodeField _  = B.empty -- should never happen 

expand :: ByteString -> Int64 -> ByteString
expand bs exLen = 
    B.append bs (B.replicate toExpand 0)
    where
        toExpand = max 0 (exLen - B.length bs)

getFields :: Tuple -> [Field]
getFields (Tuple _ _ _ fields) = fields

getTupFieldNames :: Tuple -> [FieldSpecifier]
getTupFieldNames (Tuple _ _ names _) = names

aliasTupFieldNames :: Tuple -> (String, String) -> Tuple
aliasTupFieldNames tup@(Tuple no scheme fNames fields) (old, new) = 
    Tuple no scheme (Prelude.map realias fNames) fields
    where
        realias (Implicit name) = Explicit new name
        realias (Explicit alias name) = 
            if alias == old then
                Explicit new name
            else 
                Explicit alias name