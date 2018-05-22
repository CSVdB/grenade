{-# LANGUAGE DeriveGeneric #-}

module Grenade.Utils.PositiveDouble.Internal where

import GHC.Generics

import Data.Aeson (FromJSON, ToJSON)
import Data.Validity

import Control.Monad.Catch

import Grenade.Utils.ErrToEither

newtype PositiveDouble =
    PositiveDouble Double
    deriving (Show, Eq, Generic, Ord)

instance ToJSON PositiveDouble

instance FromJSON PositiveDouble

instance Validity PositiveDouble where
    validate (PositiveDouble x) =
        mconcat
            [ delve "A PositiveDouble is a valid Double" x
            , declare "A PositiveDouble is positive" $ x >= 0
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

eitherPositiveDouble :: Double -> Either String PositiveDouble
eitherPositiveDouble = errToEither . constructPositiveDouble

instance Monoid PositiveDouble where
    mempty = PositiveDouble 0
    PositiveDouble x `mappend` PositiveDouble y = PositiveDouble $ x + y

positiveToDouble :: PositiveDouble -> Double
positiveToDouble (PositiveDouble x) = x
