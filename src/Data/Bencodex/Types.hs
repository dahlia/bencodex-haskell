{-# LANGUAGE DeriveGeneric #-}
module Data.Bencodex.Types
    ( BKey (..)
    , BValue (..)
    ) where

import GHC.Generics (Generic)

import Data.ByteString
import Data.Hashable
import Data.HashMap.Strict
import Data.Text

data BValue
    = BNull
    | BBool Bool
    | BInteger Integer
    | BByteString ByteString
    | BText Text
    | BList [BValue]
    | BMap (HashMap BKey BValue)
    deriving (Eq, Ord, Show)

data BKey
    = BTextKey Text
    | BByteStringKey ByteString
    deriving (Eq, Generic, Ord, Show)

instance Hashable BKey
