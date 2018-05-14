{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Grenade.QuickCheck.Utils.InstanceSpec where

import Test.Hspec

import Test.Grenade.Gen ()

import Grenade.Core.LearningParameters
import Grenade.Train.HyperParamInfo
import Grenade.Train.HyperParamInfo.Internal
import Grenade.Utils.Accuracy (Accuracy)
import Grenade.Utils.LogDouble (LogDouble)
import Grenade.Utils.ProperFraction (ProperFraction)

import Test.Validity
import Test.Validity.Aeson

tests :: IO Bool
tests = hspec spec >> pure True

spec :: Spec
spec = do
    genValidSpec @LearningParameters
    jsonSpecOnValid @LearningParameters
    genValidSpec @Accuracy
    jsonSpecOnValid @Accuracy
    genValidSpec @HyperParamInfo
    jsonSpecOnValid @HyperParamInfo
    genValidSpec @RunInfo
    jsonSpecOnValid @RunInfo
    genValidSpec @PositiveDouble
    jsonSpecOnValid @PositiveDouble
    genValidSpec @HyperParamInfo
    jsonSpecOnValid @HyperParamInfo
    genValidSpec @ProperFraction
    jsonSpecOnValid @ProperFraction
    genValidSpec @LogDouble
    jsonSpecOnValid @LogDouble
