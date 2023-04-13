{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Field where
import GHC.Generics
import Data.Binary

data FieldType 
    = IntT
    | StringT Int
    deriving (Show, Generic, Binary)

data Field 
    = IntF Int
    | StringF String
    | Null
    deriving (Generic, Binary)

instance Show Field where
    show (IntF x) = show x
    show (StringF s) = show s
    show Null = "NULL"