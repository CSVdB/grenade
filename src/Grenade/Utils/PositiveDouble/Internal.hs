{-# LANGUAGE DeriveGeneric #-}

module Grenade.Utils.PositiveDouble.Internal where

import GHC.Generics

import Data.Aeson (FromJSON, ToJSON)
import Data.Validity

import Control.Monad.Catch

newtype PositiveDouble =
    PositiveDouble Double
    deriving (Show, Eq, Generic, Ord)

instance ToJSON PositiveDouble

instance FromJSON PositiveDouble

instance Validity PositiveDouble where
    validate (PositiveDouble x) =
        mconcat
            [ x <?!> "A PositiveDouble is a valid Double"
            , x >= 0 <?@> "A PositiveDouble is positive"
            ]

newtype NegativePositiveDouble =
    NegativePositiveDouble String
    deriving (Show, Eq)

instance Exception NegativePositiveDouble where
    displayException (NegativePositiveDouble errMess) = errMess

constructPositiveDouble :: MonadThrow m => Double -> m PositiveDouble
constructPositiveDouble x =
    case prettyValidation $ PositiveDouble x of
        Left errMess -> throwM $ NegativePositiveDouble errMess
        Right y -> pure y

constructPosDoubleUnsafe :: Double -> PositiveDouble
constructPosDoubleUnsafe x =
    case constructPositiveDouble x of
        Left err -> error $ displayException err
        Right pd -> pd

instance Monoid PositiveDouble where
    mempty = PositiveDouble 0
    PositiveDouble x `mappend` PositiveDouble y = PositiveDouble $ x + y

positiveToDouble :: PositiveDouble -> Double
positiveToDouble (PositiveDouble x) = x
