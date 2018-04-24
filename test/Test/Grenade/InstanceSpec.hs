{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Grenade.InstanceSpec where

import Test.Hspec

import Test.Grenade.Gen ()

import Grenade.Core.LearningParameters
import Grenade.Utils.Accuracy
import Grenade.Train.HyperParams
import Grenade.Train.HyperParamInfo
import Grenade.Train.HyperParamInfo.Internal

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
    genValidSpec @DecayFactor
    jsonSpecOnValid @DecayFactor
