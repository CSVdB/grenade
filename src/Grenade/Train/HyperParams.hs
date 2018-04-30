{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Grenade.Train.HyperParams
    ( HyperParams(..)
    , prettyPrintHyperParams
    , createHyperParams
    , decay
    , DecayFactor
    , constructDecayFactor
    , dMultiply
    ) where

import Grenade.Core.LearningParameters
import Grenade.Utils.PositiveDouble.Internal

import Data.Validity
import Data.Aeson (ToJSON, FromJSON)

import GHC.Generics

data HyperParams = HyperParams
    { learningParams :: !LearningParameters
    , learningDecayFactor :: !DecayFactor -- -- rate(i) = rate * decayFactor^i, where rate*(i) is the rate during iteration i
    } deriving (Show, Eq, Generic)

instance ToJSON HyperParams

instance FromJSON HyperParams

instance Validity HyperParams

prettyPrintHyperParams :: HyperParams -> String
prettyPrintHyperParams (HyperParams LearningParameters {..} (DecayFactor df)) =
    unlines $
    fmap
        mconcat
        [ ["The learning rate is ", show $ positiveToDouble learningRate]
        , ["The momentum is ", show $ positiveToDouble learningMomentum]
        , ["The regulariser is ", show $ positiveToDouble learningRegulariser]
        , [ "The decay factor is "
          , show df
          ]
        ]

createHyperParams ::
       Double -> Double -> Double -> Double -> Either String HyperParams
createHyperParams rate momentum regulariser decayFactor =
    HyperParams <$> createLearningParameters rate momentum regulariser
      <*> prettyValidation (DecayFactor decayFactor)

newtype DecayFactor =
    DecayFactor Double
    deriving (Show, Eq, Generic)

instance ToJSON DecayFactor

instance FromJSON DecayFactor

instance Validity DecayFactor where
    validate (DecayFactor x) =
        mconcat
            [ 0 < x <?@> "A decay factor is strictly positive"
            , x < 1 <?@> "A decay factor is strictly smaller than 1"
            ]

dMultiply :: PositiveDouble -> DecayFactor -> Maybe DecayFactor
dMultiply (PositiveDouble x) (DecayFactor y) = constructDecayFactor $ x * y

constructDecayFactor :: Double -> Maybe DecayFactor
constructDecayFactor = constructValid . DecayFactor

useDecayRate :: PositiveDouble -> DecayFactor -> PositiveDouble
useDecayRate (PositiveDouble x) (DecayFactor y) = PositiveDouble $ x * y

decay :: HyperParams -> HyperParams
decay (HyperParams learnParams decayFactor) =
    flip HyperParams decayFactor $ learnParams { learningRate = useDecayRate (learningRate learnParams) decayFactor }
