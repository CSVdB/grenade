{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Grenade.Train.HyperParamInfo.Internal
    ( RunInfo(..)
    , WeightSize(..)
    , weightSize
    , HyperParamInfo(..)
    , initHyperParamInfo
    , updateHyperParamInfo
    , WeightSizeNegative
    , prettyPrintRunInfo
    , quotientOfSumOfWeights
    ) where

import Grenade.Train.HyperParams
import Grenade.Utils.Accuracy

import GHC.Generics

import Data.Aeson (FromJSON, ToJSON)

import Data.Validity

import Control.Monad.Catch

data HyperParamInfo = HyperParamInfo
    { param :: HyperParams
    , runInfo :: [RunInfo] -- The RunInfos are stored in reverse, so the first element in the list contains info about the last run.
    } deriving (Show, Eq, Generic)

instance ToJSON HyperParamInfo

instance FromJSON HyperParamInfo

instance Validity HyperParamInfo

-- This is the quotient of the sum of the weights after the last optimisation iteration
-- and the corresponding value after the first one.
quotientOfSumOfWeights :: HyperParamInfo -> WeightSize
quotientOfSumOfWeights HyperParamInfo {..} =
    let (wLast, wFirst) =
            case runInfo of
                (x:xs) ->
                    case reverse xs of
                        (y:_) -> (sizeOfWeights x, sizeOfWeights y)
                        _ -> error "There is only one element in runinfo"
                _ -> error "There are no runinfos"
     in WeightSize $ weight wLast / weight wFirst

initHyperParamInfo :: HyperParams -> HyperParamInfo
initHyperParamInfo param = HyperParamInfo param []

updateHyperParamInfo :: RunInfo -> HyperParamInfo -> HyperParamInfo
updateHyperParamInfo info HyperParamInfo {..} =
    HyperParamInfo param $ info : runInfo

-- The info you can collect from one iteration of training (over a training
-- data set) given a set of learningparameters.
data RunInfo = RunInfo
    { trainAccuracy :: Accuracy
    , validationAccuracy :: Accuracy
    , sizeOfWeights :: WeightSize
    , changeOfWeights :: WeightSize
    } deriving (Show, Eq, Generic)

instance ToJSON RunInfo

instance FromJSON RunInfo

instance Validity RunInfo

prettyPrintRunInfo :: RunInfo -> String
prettyPrintRunInfo RunInfo {..} =
    mconcat
        [ showAccuracy "train" trainAccuracy
        , showAccuracy "validation" validationAccuracy
        , showSizeOfWeights sizeOfWeights
        , showChangeOfWeights changeOfWeights
        ]

newtype WeightSize = WeightSize
    { weight :: Double
    } deriving (Show, Eq, Generic, Ord)

data WeightSizeNegative =
    WeightSizeNegative
    deriving (Show, Eq, Generic)

instance Exception WeightSizeNegative where
    displayException _ = "There is a negative sum squared of weights."

-- The sum of the squares of the weights, or of the (delta weight)s.
weightSize :: MonadThrow m => Double -> m WeightSize
weightSize x =
    case x >= 0 of
        True -> pure $ WeightSize x
        False -> throwM WeightSizeNegative

showSizeOfWeights :: WeightSize -> String
showSizeOfWeights (WeightSize x) =
    "The size of the weights is " ++ show x ++ "."

showChangeOfWeights :: WeightSize -> String
showChangeOfWeights (WeightSize x) =
    "The size of the change in weights is " ++ show x ++ "."

instance ToJSON WeightSize

instance FromJSON WeightSize

instance Validity WeightSize where
    validate (WeightSize x) =
        x >= 0 <?@> "The sum of squared weights is positive."

instance Monoid WeightSize where
    mempty = WeightSize 0
    WeightSize x `mappend` WeightSize y = WeightSize $ x + y
