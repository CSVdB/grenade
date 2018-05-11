{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Grenade.Core.LearningParameters
Description : Stochastic gradient descent learning parameters
Copyright   : (c) Huw Campbell, 2016-2017
License     : BSD2
Stability   : experimental
-}
module Grenade.Core.LearningParameters
    ( LearningParameters(..)
    , createLearningParameters
    , positiveToDouble
    , PositiveDouble
    ) where

-- | This module contains learning algorithm specific
--   code. Currently, this module should be considered
--   unstable, due to issue #26.
import Grenade.Utils.PositiveDouble
import Grenade.Utils.ProperFraction

import GHC.Generics

import Data.Aeson (FromJSON, ToJSON)

import Data.Validity

-- | Learning parameters for stochastic gradient descent.
data LearningParameters = LearningParameters
    { learningRate :: !PositiveDouble
    , learningMomentum :: !ProperFraction
    , learningRegulariser :: !PositiveDouble
    } deriving (Eq, Show, Generic)

instance ToJSON LearningParameters

instance FromJSON LearningParameters

instance Validity LearningParameters

createLearningParameters ::
       Double -> Double -> Double -> Either String LearningParameters
createLearningParameters rate momentum regulariser =
    LearningParameters <$> eitherPositiveDouble rate <*>
    eitherProperFraction momentum <*>
    eitherPositiveDouble regulariser
