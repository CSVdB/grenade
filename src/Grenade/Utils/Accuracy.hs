{-# LANGUAGE DeriveGeneric #-}

module Grenade.Utils.Accuracy
    ( Accuracy
    , accuracyM
    , showAccuracy
    ) where

import Data.Validity

import Data.Aeson (FromJSON, ToJSON)

import GHC.Generics

import Control.Monad.Catch

newtype Accuracy =
    Accuracy Double
    deriving (Show, Eq, Generic)

data AccuracyOutOfBounds =
    AccuracyOutOfBounds
    deriving (Show, Eq, Generic)

instance Exception AccuracyOutOfBounds where
    displayException = const "The accuracy is not in [0,1]."

accuracyM :: MonadThrow m => Double -> m Accuracy
accuracyM x =
    case prettyValidation $ Accuracy x of
        Right a -> pure a
        Left _ -> throwM AccuracyOutOfBounds

instance ToJSON Accuracy

instance FromJSON Accuracy

instance Validity Accuracy where
    validate (Accuracy x) = 0 <= x && x <= 1 <?@> "The accuracy is in [0,1]"

showAccuracy :: String -> Accuracy -> String
showAccuracy name (Accuracy x) =
    "The " ++ name ++ " accuracy is " ++ show (nOfPrecent * x) ++ "%."

nOfPrecent :: Double
nOfPrecent = 100
