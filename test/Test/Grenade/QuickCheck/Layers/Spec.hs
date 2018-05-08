{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Test.Grenade.QuickCheck.Layers.Spec
    ( layerSpec
    , tests
    ) where

import Test.Grenade.QuickCheck.Gen
import TestUtils

import Grenade
import Grenade.Utils.SumSquaredParams

import Data.Either
import Data.GenValidity
import Data.Proxy

import GHC.TypeLits

import Test.Hspec
import Test.QuickCheck

import Test.Grenade.QuickCheck.Layers.Gen ()

tests :: IO Bool
tests = toTests spec

type O2 = 2 * O

type O2shape = 'D1 O2

spec :: Spec
spec = do
    layerSpec
        @(Concat Oshape (FullyConnected I O) Oshape (FullyConnected I O))
        @Ishape
        @O2shape
    layerSpec @(Convolution 1 1 5 5 3 3) @('D2 29 29) @('D2 9 9)
    layerSpec @(FullyConnected I O) @Ishape @Oshape
    layerSpec @Logit @Ishape @Ishape
    layerSpec @Reshape @Image @Ishape
    layerSpec @Tanh @Image @Image

layerSpec ::
       forall x (i :: Shape) (o :: Shape).
       ( Layer x i o
       , GenValid (S i)
       , GenValid (S o)
       , GenValid x
       , GenValid (Gradient x)
       , GenValid (Tape x i o)
       , Show x
       , Show (Gradient x)
       , Show (Tape x i o)
       , Validity (Tape x i o)
       , SumSquaredParams x
       )
    => Spec
layerSpec = do
    describe "runForwards" $
        it "creates valid output" $
        forAll genValid $ \(inpt :: S i) ->
            forAll genValid $ \(layer :: x) ->
                let (tape, outpt :: S o) = runForwards layer inpt
                 in do prettyValidation tape `shouldSatisfy` isRight
                       prettyValidation outpt `shouldSatisfy` isRight
    describe "runBackwards" $
        it "creates valid output" $
        forAll genValid $ \(tape :: Tape x i o) ->
            forAll genValid $ \(layer :: x) ->
                forAll genValid $ \(outpt :: S o) ->
                    let (grad, inpt :: S i) = runBackwards layer tape outpt
                     in (validate grad, validate inpt) `shouldBe` (valid, valid)
    describe "runUpdate" $
        it "creates valid output" $
        forAll genValid $ \(lParams :: LearningParameters) ->
            forAll genValid $ \(layer :: x) ->
                forAll genValid $ \(grad :: Gradient x) ->
                    let newFcl = runUpdate lParams layer grad
                     in validate newFcl `shouldBe` valid
    describe "getSumSquaredParams" $
        it "creates valid output" $
        forAll genValid $ \(layer :: x) -> do
            let x = getSumSquaredParams layer
            validate x `shouldBe` valid
    describe "getSumSquaredParamsDelta" $
        it "creates valid output" $
        forAll genValid $ \(grad :: Gradient x) -> do
            let x = getSumSquaredParamsDelta (Proxy :: Proxy x) grad
            validate x `shouldBe` valid
