{-|
Module      : Grenade.Core.LearningParameters
Description : Stochastic gradient descent learning parameters
Copyright   : (c) Huw Campbell, 2016-2017
License     : BSD2
Stability   : experimental
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Grenade.Core.LearningParameters (
  -- | This module contains learning algorithm specific
  --   code. Currently, this module should be considered
  --   unstable, due to issue #26.

    LearningParameters (..)
  ) where

import GHC.Generics

import Data.Aeson (ToJSON, FromJSON)

import Data.Validity

-- | Learning parameters for stochastic gradient descent.
data LearningParameters = LearningParameters {
    learningRate :: Double
  , learningMomentum :: Double
  , learningRegulariser :: Double
  , learningDecayFactor :: Double -- rate(i) = rate * decayFactor^i, where rate*(i) is the rate during iteration i
  } deriving (Eq, Show, Generic)

instance ToJSON LearningParameters

instance FromJSON LearningParameters

instance Validity LearningParameters where
    validate LearningParameters {..} =
        mconcat
            [ learningRate > 0 <?@> "The learning rate is strictly positive"
            , learningMomentum >= 0 <?@> "The momentum parameter is positive"
            , learningRegulariser >= 0 <?@> "The regulariser is positive"
            , learningDecayFactor > 0 <?@> "The decay rate is strictly positive"
            , learningDecayFactor < 1 <?@> "The decay rate is strictly smaller than 1"
            ]
