{-# LANGUAGE TypeApplications #-}

module Test.Grenade.QuickCheck.Train.HyperParamsSpec
    ( tests
    ) where

import Grenade.Train.HyperParams

import TestUtils

import Test.Hspec

import Test.Validity

import Test.Grenade.Gen ()

tests :: IO Bool
tests = toTests spec

spec :: Spec
spec =
    describe "decay" $
    it "produces valid output" $ forAllValid $ shouldBeValid . decay
