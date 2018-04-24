{-# LANGUAGE DeriveGeneric #-}

module Grenade.Utils.PositiveDouble.Internal where

import GHC.Generics

import Data.Aeson (FromJSON, ToJSON)
import Data.Validity

newtype PositiveDouble =
    PositiveDouble Double
    deriving (Show, Eq, Generic)

instance ToJSON PositiveDouble

instance FromJSON PositiveDouble

instance Validity PositiveDouble where
    validate (PositiveDouble x) = x >= 0 <?@> "A PositiveDouble is positive"
