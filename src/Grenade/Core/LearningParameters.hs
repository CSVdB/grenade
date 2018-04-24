{-|
Module      : Grenade.Core.LearningParameters
Description : Stochastic gradient descent learning parameters
Copyright   : (c) Huw Campbell, 2016-2017
License     : BSD2
Stability   : experimental
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Grenade.Core.LearningParameters
  -- | This module contains learning algorithm specific
  --   code. Currently, this module should be considered
  --   unstable, due to issue #26.
    ( LearningParameters(..)
    , createLearningParameters
    , positiveToDouble
    , PositiveDouble
    ) where

import Grenade.Utils.PositiveDouble
import Grenade.Utils.PositiveDouble.Internal

import GHC.Generics

import Data.Aeson (FromJSON, ToJSON)

import Data.Validity

-- | Learning parameters for stochastic gradient descent.
data LearningParameters = LearningParameters
    { learningRate :: PositiveDouble
    , learningMomentum :: PositiveDouble
    , learningRegulariser :: PositiveDouble
    } deriving (Eq, Show, Generic)

instance ToJSON LearningParameters

instance FromJSON LearningParameters

instance Validity LearningParameters

createLearningParameters ::
       Double -> Double -> Double -> Either String LearningParameters
createLearningParameters rate momentum regulariser =
    LearningParameters <$> prettyValidation (PositiveDouble rate) <*>
    prettyValidation (PositiveDouble momentum) <*>
    prettyValidation (PositiveDouble regulariser)
