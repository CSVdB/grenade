{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Grenade.Gen where

import Grenade.Core.LearningParameters

import Grenade.Train.HyperParamInfo
import Grenade.Train.HyperParamInfo.Internal
import Grenade.Train.HyperParams
import Grenade.Utils.PositiveDouble.Internal

import Data.GenValidity
import Data.Maybe

import Test.QuickCheck (choose, listOf)

instance GenUnchecked PositiveDouble

instance GenValid PositiveDouble where
    genValid = PositiveDouble . abs <$> genValid

instance GenUnchecked LearningParameters

instance GenValid LearningParameters where
    genValid = LearningParameters <$> genValid <*> genValid <*> genValid

instance GenUnchecked Accuracy

instance GenValid Accuracy where
    genValid = fromMaybe <$> genValid <*> (accuracyM <$> choose (0, 1))

instance GenUnchecked RunInfo

instance GenValid RunInfo where
    genValid = RunInfo <$> genValid <*> genValid <*> genValid <*> genValid

instance GenUnchecked HyperParamInfo

instance GenValid HyperParamInfo where
    genValid = HyperParamInfo <$> genValid <*> listOf genValid

instance GenUnchecked HyperParams

instance GenValid HyperParams where
    genValid = HyperParams <$> genValid <*> genValid

instance GenUnchecked DecayFactor

instance GenValid DecayFactor where
    genValid =
        fromMaybe <$> genValid <*> (constructDecayFactor <$> choose (0, 1))
