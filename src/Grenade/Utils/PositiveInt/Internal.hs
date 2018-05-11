{-# LANGUAGE DeriveGeneric #-}

module Grenade.Utils.PositiveInt.Internal where

import GHC.Generics

import Data.Aeson (FromJSON, ToJSON)
import Data.Validity

import Control.Monad.Catch

newtype PositiveInt =
    PositiveInt Int
    deriving (Show, Eq, Generic, Ord)

instance ToJSON PositiveInt

instance FromJSON PositiveInt

instance Validity PositiveInt where
    validate (PositiveInt n) = n > 0 <?@> "A PositiveInt is strictly positive"

newtype NegativePositiveInt =
    NegativePositiveInt String
    deriving (Show, Eq)

instance Exception NegativePositiveInt where
    displayException (NegativePositiveInt errMess) = errMess

constructPositiveInt :: MonadThrow m => Int -> m PositiveInt
constructPositiveInt n =
    case prettyValidation $ PositiveInt n of
        Left errMess -> throwM $ NegativePositiveInt errMess
        Right y -> pure y
