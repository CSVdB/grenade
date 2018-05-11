{-# LANGUAGE DeriveGeneric #-}

module Grenade.Utils.DecayRate
    ( ProperFraction
    , constructProperFraction
    , dMultiply
    , decayToDouble
    , useDecayRate
    ) where

import Grenade.Utils.PositiveDouble.Internal

import Data.Aeson (FromJSON, ToJSON)
import Data.Validity

import Control.Monad.Catch

import GHC.Generics

newtype ProperFraction =
    ProperFraction Double
    deriving (Show, Eq, Generic)

instance ToJSON ProperFraction

instance FromJSON ProperFraction

instance Validity ProperFraction where
    validate (ProperFraction x) =
        mconcat
            [ 0 < x <?@> "A decay factor is strictly positive"
            , x < 1 <?@> "A decay factor is strictly smaller than 1"
            ]

dMultiply :: PositiveDouble -> ProperFraction -> Maybe ProperFraction
dMultiply x (ProperFraction y) = constructProperFraction $ (positiveToDouble x) * y

newtype ExceptionProperFraction =
    ExceptionProperFraction String
    deriving (Show, Eq)

instance Exception ExceptionProperFraction where
    displayException (ExceptionProperFraction errMess) = errMess

constructProperFraction :: MonadThrow m => Double -> m ProperFraction
constructProperFraction x =
    case prettyValidation $ ProperFraction x of
        Left err -> throwM $ ExceptionProperFraction err
        Right y -> pure y

useDecayRate :: PositiveDouble -> ProperFraction -> PositiveDouble
useDecayRate x (ProperFraction y) = PositiveDouble $ (positiveToDouble x) * y

decayToDouble :: ProperFraction -> Double
decayToDouble (ProperFraction x) = x
