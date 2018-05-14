{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Grenade.QuickCheck.Utils.Gen where

import Grenade.Core.LearningParameters

import Grenade.Train.HyperParamInfo
import Grenade.Train.HyperParamInfo.Internal
import Grenade.Train.HyperParams
import Grenade.Utils.LogDouble
import Grenade.Utils.PositiveDouble.Internal
import Grenade.Utils.PositiveInt.Internal
import Grenade.Utils.ProperFraction

import Data.GenValidity
import Data.Maybe

import Test.QuickCheck (choose, listOf)

instance GenUnchecked PositiveDouble

instance GenValid PositiveDouble where
    genValid =
        fromMaybe <$> genValid <*> (constructPositiveDouble . abs <$> genValid)

instance GenUnchecked PositiveInt

instance GenValid PositiveInt where
    genValid =
        fromMaybe <$> genValid <*> (constructPositiveInt . abs <$> genValid)

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

instance GenUnchecked ProperFraction

instance GenValid ProperFraction where
    genValid =
        fromMaybe <$> genValid <*> (constructProperFraction <$> choose (0, 1))

instance GenUnchecked LogDouble

instance GenValid LogDouble where
    genValid =
        fromMaybe <$> genValid <*>
        (eitherToMaybe . constructLogDouble <$> choose (0, maxLogDouble))

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x
