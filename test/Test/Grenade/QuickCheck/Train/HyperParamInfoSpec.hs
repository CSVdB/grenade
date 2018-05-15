{-# LANGUAGE TypeApplications #-}

module Test.Grenade.QuickCheck.Train.HyperParamInfoSpec
    ( tests
    ) where

import Grenade.Train.HyperParamInfo
import Grenade.Train.HyperParamInfo.Internal

import TestUtils

import Test.Hspec

import Test.QuickCheck
import Test.Validity

import Test.Grenade.Gen ()

tests :: IO Bool
tests = toTests spec

spec :: Spec
spec = do
    describe "getHyperParamInfo" $
        it "produces valid output" $
        forAll posIntGen $ \epochs ->
            forAllValid @NN $ \net ->
                forAllValid $ \trainSet ->
                    forAllValid $ \valSet ->
                        forAllValid $ \params ->
                            shouldBeValid $
                            getHyperParamInfo epochs net trainSet valSet params
    describe "getAccuracy" $
        it "produces valid output" $
        forAllValid $ \h ->
            case h of
                HyperParamInfo _ [] -> pure ()
                hpi -> shouldBeValid $ getAccuracy hpi
