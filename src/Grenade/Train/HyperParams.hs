{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Grenade.Train.HyperParams
    ( HyperParams(..)
    , prettyPrintHyperParams
    , createHyperParams
    , decay
    ) where

import Grenade.Core.LearningParameters
import Grenade.Utils.ProperFraction

import Data.Aeson (FromJSON, ToJSON)
import Data.Validity

import GHC.Generics

import Control.Monad.Catch

data HyperParams = HyperParams
    { learningParams :: !LearningParameters
    , learningProperFraction :: !ProperFraction -- -- rate(i) = rate * properFraction^i, where rate*(i) is the rate during iteration i
    } deriving (Show, Eq, Generic)

instance ToJSON HyperParams

instance FromJSON HyperParams

instance Validity HyperParams

prettyPrintHyperParams :: HyperParams -> String
prettyPrintHyperParams (HyperParams LearningParameters {..} df) =
    unlines $
    fmap
        mconcat
        [ ["The learning rate is ", show $ positiveToDouble learningRate]
        , ["The momentum is ", show $ positiveToDouble learningMomentum]
        , ["The regulariser is ", show $ positiveToDouble learningRegulariser]
        , ["The decay factor is ", show $ decayToDouble df]
        ]

createHyperParams ::
       Double -> Double -> Double -> Double -> Either String HyperParams
createHyperParams rate momentum regulariser properFraction = do
    df <-
        case constructProperFraction properFraction of
            Left err -> Left $ displayException err
            Right dfr -> Right dfr
    HyperParams <$> createLearningParameters rate momentum regulariser <*>
        prettyValidation df

decay :: HyperParams -> HyperParams
decay (HyperParams learnParams properFraction) =
    flip HyperParams properFraction $
    learnParams
        {learningRate = useDecayRate (learningRate learnParams) properFraction}
