{-# LANGUAGE DeriveGeneric #-}

module Grenade.Utils.PositiveDouble.Internal where

import GHC.Generics

import Data.Aeson (FromJSON, ToJSON)
import Data.Validity

import Control.Monad.Catch

newtype PositiveDouble =
    PositiveDouble Double
    deriving (Show, Eq, Generic)

instance ToJSON PositiveDouble

instance FromJSON PositiveDouble

instance Validity PositiveDouble where
    validate (PositiveDouble x) = x >= 0 <?@> "A PositiveDouble is positive"

data NegativePositiveDouble = NegativePositiveDouble deriving (Show, Eq)

instance Exception NegativePositiveDouble where
    displayException NegativePositiveDouble = "A PositiveDouble must be positive."

constructPositiveDouble :: MonadThrow m => Double -> m PositiveDouble
constructPositiveDouble x = case constructValid $ PositiveDouble x of
    Nothing -> throwM NegativePositiveDouble
    Just y -> pure y

instance Monoid PositiveDouble where
    mempty = PositiveDouble 0
    PositiveDouble x `mappend` PositiveDouble y = PositiveDouble $ x + y
