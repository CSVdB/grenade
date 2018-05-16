{-# LANGUAGE TypeApplications #-}

module Test.Grenade.QuickCheck.Train.NetworkSpec
    ( tests
    ) where

import Grenade.Train.Network

import TestUtils

import Test.Hspec
import Test.QuickCheck
import Test.Validity

import Test.Grenade.Gen ()

tests :: IO Bool
tests = toTests spec

spec :: Spec
spec = do
    describe "runIteration" $
        it "produces valid output" $
        forAllValid $ \params ->
            forAllValid $ \dataset ->
                forAllValid @NN $ \net ->
                    shouldBeValid $ runIteration params dataset net
    describe "trainNetwork" $ do
        it "produces valid output" $
            forAll posIntGen $ \epochs ->
                forAllValid $ \params ->
                    forAllValid $ \dataset ->
                        forAllValid @NN $ \net ->
                            shouldBeValid $
                            trainNetwork epochs params dataset net
    describe "getNetAndRunInfo" $
        it "produces valid output" $
        forAllValid $ \params ->
            forAllValid $ \trainSet ->
                forAllValid $ \valSet ->
                    forAllValid @NN $ \net ->
                        shouldBeValid $
                        getNetAndRunInfo params trainSet valSet net
