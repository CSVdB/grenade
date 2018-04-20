{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Grenade.Train.LearningParameters.Internal
    ( RunInfo(..)
    , WeightSize
    , weightSize
    , HyperParamInfo(..)
    , initHyperParamInfo
    , updateHyperParamInfo
    , WeightSizeNegative
    , prettyPrintRunInfo
    ) where

import Grenade.Core.LearningParameters
import Grenade.Utils.Accuracy

import GHC.Generics

import Data.Aeson (ToJSON, FromJSON)

import Data.Validity

import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Foldable

data HyperParamInfo = HyperParamInfo
    { param :: LearningParameters
    , runInfo :: [RunInfo] -- The RunInfos are stored in reverse, so the first element in the list contains info about the last run.
    } deriving (Show, Eq, Generic)

instance ToJSON HyperParamInfo

instance FromJSON HyperParamInfo

instance Validity HyperParamInfo

initHyperParamInfo :: LearningParameters -> HyperParamInfo
initHyperParamInfo param = HyperParamInfo param []

updateHyperParamInfo :: RunInfo -> HyperParamInfo -> HyperParamInfo
updateHyperParamInfo info HyperParamInfo {..} = HyperParamInfo param $ info : runInfo

-- The info you can collect from one iteration of training (over a training
-- data set) given a set of learningparameters.
data RunInfo = RunInfo
    { trainAccuracy :: Accuracy
    , validationAccuracy :: Accuracy
    , testAccuracy :: Accuracy
    , sizeOfWeights :: WeightSize
    , changeOfWeights :: WeightSize
    } deriving (Show, Eq, Generic)

instance ToJSON RunInfo

instance FromJSON RunInfo

instance Validity RunInfo

prettyPrintRunInfo :: MonadIO m => RunInfo -> m ()
prettyPrintRunInfo RunInfo {..} = liftIO $ traverse_ putStrLn
        [ showAccuracy "train" trainAccuracy
        , showAccuracy "validation" validationAccuracy
        , showAccuracy "test" testAccuracy
        , showSizeOfWeights sizeOfWeights
        , showChangeOfWeights changeOfWeights
        ]

newtype WeightSize = WeightSize Double deriving (Show, Eq, Generic)

data WeightSizeNegative = WeightSizeNegative deriving (Show, Eq, Generic)

instance Exception WeightSizeNegative where
    displayException _ = "There is a negative sum squared of weights."

-- The sum of the squares of the weights, or of the (delta weight)s.
weightSize :: MonadThrow m => Double -> m WeightSize
weightSize x = case x >= 0 of
    True -> pure $ WeightSize x
    False -> throwM WeightSizeNegative

showSizeOfWeights :: WeightSize -> String
showSizeOfWeights (WeightSize x) = "The size of the weights is " ++ show x ++ "."

showChangeOfWeights :: WeightSize -> String
showChangeOfWeights (WeightSize x) = "The size of the change in weights is " ++ show x ++ "."

instance ToJSON WeightSize

instance FromJSON WeightSize

instance Validity WeightSize where
    validate (WeightSize x) = x >= 0 <?@> "The sum of squared weights is positive."

instance Monoid WeightSize where
    mempty = WeightSize 0
    WeightSize x `mappend` WeightSize y = WeightSize $ x + y
