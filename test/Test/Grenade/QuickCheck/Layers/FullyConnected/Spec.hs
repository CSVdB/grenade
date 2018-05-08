{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Grenade.QuickCheck.Layers.FullyConnected.Spec
    ( tests
    ) where

import Grenade
import Grenade.Utils.SumSquaredParams

import Data.Proxy
import Data.Validity (validate)
import Test.Grenade.Gen ()
import Test.Grenade.QuickCheck.Layers.FullyConnected.Gen ()
import Test.Hspec
import Test.Hspec.Runner
import Test.QuickCheck
import Test.Validity

tests :: IO Bool
tests = (== 0) . summaryFailures <$> hspecResult spec

type I = 10

type O = 28

type InptShape = 'D1 I

type OutptShape = 'D1 O

type Inpt = S InptShape

type Outpt = S OutptShape

type FCL = FullyConnected I O

type FCLTape = Tape FCL InptShape OutptShape

type Grad = Gradient FCL

valid :: Validation
valid = Validation []

spec :: Spec
spec = do
    describe "runForwards" $
        it "creates valid output" $
        forAll (S1D <$> genValid) $ \(inpt :: Inpt) ->
            forAll genValid $ \(layer :: FCL) ->
                let (tape, outpt :: Outpt) = runForwards layer inpt
                 in (validate tape, validate outpt) `shouldBe` (valid, valid)
    describe "runBackwards" $
        it "creates valid output" $
        forAll genValid $ \(tape :: FCLTape) ->
            forAll genValid $ \(layer :: FCL) ->
                forAll (S1D <$> genValid) $ \(outpt :: Outpt) ->
                    let (grad, inpt :: Inpt) = runBackwards layer tape outpt
                     in (validate grad, validate inpt) `shouldBe` (valid, valid)
    describe "runUpdate" $
        it "creates valid output" $
        forAll genValid $ \(lParams :: LearningParameters) ->
            forAll genValid $ \(layer :: FCL) ->
                forAll genValid $ \(grad :: Grad) ->
                    let newFcl = runUpdate lParams layer grad
                     in validate newFcl `shouldBe` valid
    describe "getSumSquaredParams" $
        it "creates valid output" $
        forAll genValid $ \(fcl :: FCL) -> do
            let x = getSumSquaredParams fcl
            validate x `shouldBe` valid
    describe "getSumSquaredParamsDelta" $
        it "creates valid output" $
        forAll genValid $ \(grad :: Grad) -> do
            let x = getSumSquaredParamsDelta (Proxy :: Proxy FCL) grad
            validate x `shouldBe` valid
