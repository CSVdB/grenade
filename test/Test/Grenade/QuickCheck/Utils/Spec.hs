{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Grenade.QuickCheck.Utils.Spec where

import Test.Hspec

import Test.Grenade.QuickCheck.Utils.Gen ()

import Grenade.Utils.Accuracy
import Grenade.Utils.LogDouble
import Grenade.Utils.PositiveDouble
import Grenade.Utils.PositiveInt
import Grenade.Utils.ProperFraction

import Test.Validity

import Data.List.NonEmpty

tests :: IO Bool
tests = hspec spec >> pure True

spec :: Spec
spec = do
    describe "accuracyM" $
        it "produces valid outcome" $
        forAllValid $ shouldBeValid . accuracyM @Maybe
    describe "showAccuracy" $
        it "produces valid outcome" $
        forAllValid $ \string ->
            forAllValid $ \acc -> shouldBeValid $ showAccuracy string acc
    describe "pExp" $
        it "produces valid outcome" $
        forAllValid $ \posD ->
            forAllValid $ \logD -> shouldBeValid $ pExp posD logD
    describe "constructLogDouble" $
        it "produces valid outcome" $
        forAllValid $ shouldBeValid . constructLogDouble
    describe "decayLogDouble" $
        it "produces valid outcome" $
        forAllValid $ \logD ->
            forAllValid $ \properFrac ->
                shouldBeValid $ decayLogDouble logD properFrac
    describe "pMultiply" $
        it "produces valid outcome" $
        forAllValid $ \posD ->
            forAllValid $ \posD' -> shouldBeValid $ pMultiply posD posD'
    describe "pMultiply" $
        it "produces valid outcome" $
        forAllValid $ \posD ->
            forAllValid $ \posD' -> shouldBeValid $ pMultiply posD posD'
    describe "takePos" $
        it "produces valid outcome" $
        forAllValid $ \posInt ->
            forAllValid @(NonEmpty Int) $ \ne ->
                shouldBeValid $ takePos posInt ne
    describe "dMultiply" $
        it "produces valid outcome" $
        forAllValid $ \posD ->
            forAllValid $ \properFrac ->
                shouldBeValid $ dMultiply posD properFrac
    describe "useDecayRate" $
        it "produces valid outcome" $
        forAllValid $ \posD ->
            forAllValid $ \properFrac ->
                shouldBeValid $ useDecayRate posD properFrac
    describe "properFractionMultiply" $
        it "produces valid outcome" $
        forAllValid $ \posD ->
            forAllValid $ \properFrac ->
                shouldBeValid $ properFractionMultiply posD properFrac
