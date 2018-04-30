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
    validate (PositiveDouble x) = mconcat
        [ x <?!> "A PositiveDouble is a valid Double"
        , x >= 0 <?@> "A PositiveDouble is positive"
        ]

newtype NegativePositiveDouble = NegativePositiveDouble String deriving (Show, Eq)

instance Exception NegativePositiveDouble where
    displayException (NegativePositiveDouble errMess) = errMess

constructPositiveDouble :: MonadThrow m => Double -> m PositiveDouble
constructPositiveDouble x = case prettyValidation $ PositiveDouble x of
    Left errMess -> throwM $ NegativePositiveDouble errMess
    Right y -> pure y

instance Monoid PositiveDouble where
    mempty = PositiveDouble 0
    PositiveDouble x `mappend` PositiveDouble y = PositiveDouble $ x + y
