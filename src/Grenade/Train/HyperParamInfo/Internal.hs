{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Grenade.Train.HyperParamInfo.Internal
    ( RunInfo(..)
    , HyperParamInfo(..)
    , initHyperParamInfo
    , updateHyperParamInfo
    , prettyPrintRunInfo
    , quotientOfParamsSum
    ) where

import Grenade.Train.HyperParams
import Grenade.Utils.Accuracy
import Grenade.Utils.PositiveDouble
import Grenade.Utils.PositiveDouble.Internal

import GHC.Generics

import Data.Aeson (FromJSON, ToJSON)

import Data.Validity

data HyperParamInfo = HyperParamInfo
    { param :: !HyperParams
    , runInfo :: ![RunInfo] -- The RunInfos are stored in reverse, so the first element in the list contains info about the last run.
    } deriving (Show, Eq, Generic)

instance ToJSON HyperParamInfo

instance FromJSON HyperParamInfo

instance Validity HyperParamInfo

-- This is the quotient of the sum of the weights after the last optimisation iteration
-- and the corresponding value after the first one.
-- Returns 1 if there are no runInfos or only one.
quotientOfParamsSum :: HyperParamInfo -> PositiveDouble
quotientOfParamsSum HyperParamInfo {..} =
    case runInfo of
        (x:xs) ->
            let wLast = sizeOfWeights x
             in case reverse xs of
                    (y:_) ->
                        let wFirst = sizeOfWeights y
                         in pDivide wLast wFirst
                    _ -> PositiveDouble 1
        _ -> PositiveDouble 1

initHyperParamInfo :: HyperParams -> HyperParamInfo
initHyperParamInfo param = HyperParamInfo param []

updateHyperParamInfo :: RunInfo -> HyperParamInfo -> HyperParamInfo
updateHyperParamInfo info h = h {runInfo = info : runInfo h}

-- The info you can collect from one iteration of training (over a training
-- data set) given a set of learningparameters.
data RunInfo = RunInfo
    { trainAccuracy :: !Accuracy
    , validationAccuracy :: !Accuracy
    , sizeOfWeights :: !PositiveDouble
    , changeOfWeights :: !PositiveDouble
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

showSizeOfWeights :: PositiveDouble -> String
showSizeOfWeights (PositiveDouble x) =
    "The size of the weights is " ++ show x ++ "."

showChangeOfWeights :: PositiveDouble -> String
showChangeOfWeights (PositiveDouble x) =
    "The size of the change in weights is " ++ show x ++ "."
