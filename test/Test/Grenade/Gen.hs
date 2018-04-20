{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Grenade.Gen where

import Grenade.Core.LearningParameters

import Grenade.Utils.Accuracy
import Grenade.Train.LearningParameters
import Grenade.Train.LearningParameters.Internal

import Data.GenValidity
import Data.Maybe

import Test.QuickCheck (choose, listOf)
import Test.QuickCheck.Gen (suchThat)

instance GenUnchecked LearningParameters

instance GenValid LearningParameters where
    genValid = do
        rate <- genValid `suchThat` (> 0)
        momentum <- genValid `suchThat` (>= 0)
        LearningParameters rate momentum <$> genValid `suchThat` (>= 0)

instance GenUnchecked Accuracy

instance GenValid Accuracy where
    genValid = fromMaybe <$> genValid <*> (accuracyM <$> choose (0,1))

instance GenUnchecked WeightSize

instance GenValid WeightSize where
    genValid = fromMaybe <$> genValid <*> (weightSize . abs <$> genValid)

instance GenUnchecked RunInfo

instance GenValid RunInfo where
    genValid = RunInfo <$> genValid <*> genValid <*> genValid <*> genValid <*> genValid

instance GenUnchecked HyperParamInfo

instance GenValid HyperParamInfo where
    genValid = HyperParamInfo <$> genValid <*> listOf genValid
