{-# LANGUAGE DeriveGeneric #-}

module Grenade.Utils.ProperFraction
    ( ProperFraction
    , constructProperFraction
    , dMultiply
    , properToDouble
    , useDecayRate
    , eitherProperFraction
    , properToPosDouble
    , properFractionMultiply
    ) where

import Grenade.Utils.ErrToEither
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
            [ delve "A proper fraction is a valid double" x
            , declare "A proper fraction is strictly positive" $ x > 0
            , declare "A proper fraction is strictly smaller than 1" $ x < 1
            ]

dMultiply :: PositiveDouble -> ProperFraction -> Maybe ProperFraction
dMultiply x (ProperFraction y) =
    constructProperFraction $ (positiveToDouble x) * y

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

eitherProperFraction :: Double -> Either String ProperFraction
eitherProperFraction = errToEither . constructProperFraction

useDecayRate :: PositiveDouble -> ProperFraction -> PositiveDouble
useDecayRate x (ProperFraction y) = PositiveDouble $ (positiveToDouble x) * y

properToDouble :: ProperFraction -> Double
properToDouble (ProperFraction x) = x

properToPosDouble :: ProperFraction -> PositiveDouble
properToPosDouble (ProperFraction x) = PositiveDouble x

properFractionMultiply :: PositiveDouble -> ProperFraction -> ProperFraction
properFractionMultiply (PositiveDouble x) (ProperFraction y) =
    case prettyValidation . ProperFraction $ x * y of
        Left _ -> ProperFraction y
        Right z -> z
