{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Test.Grenade.QuickCheck.Layers.LayerSpec
    ( layerSpec
    ) where

import Grenade

import Control.Monad.Random
import Data.GenValidity
import Data.Proxy
import Data.Singletons
import Data.Typeable

import Test.Hspec
import Test.Validity

import Test.Grenade.QuickCheck.Layers.Gen ()

layerSpec ::
       forall x (i :: Shape) (o :: Shape).
       ( Layer x i o
       , SingI i
       , SingI o
       , GenValid x
       , GenValid (Gradient x)
       , GenValid (Tape x i o)
       , Show x
       , Show (Gradient x)
       , Show (Tape x i o)
       , Validity (Tape x i o)
       , MetricNormedSpace x
       , Typeable x
       , Typeable (Gradient x)
       , Typeable i
       , Typeable o
       )
    => Spec
layerSpec = do
    genValidSpec @x
    genValidSpec @(Gradient x)
    describeWith "createRandom" $
        it "creates valid output" $
        forAllValid $ \seed ->
            shouldBeValid $ evalRand (createRandom @x) $ mkStdGen seed
    describeWith "runForwards" $
        it "creates valid output" $
        forAllValid $ \(inpt :: S i) ->
            forAllValid $ \(layer :: x) ->
                shouldBeValid (runForwards layer inpt :: (Tape x i o, S o))
    describeWith "runBackwards" $
        it "creates valid output" $
        forAllValid $ \(tape :: Tape x i o) ->
            forAllValid $ \(layer :: x) ->
                forAllValid $ \(outpt :: S o) ->
                    shouldBeValid
                        (runBackwards layer tape outpt :: (Gradient x, S i))
    describeWith "runUpdate" $
        it "creates valid output" $
        forAllValid $ \(lParams :: LearningParameters) ->
            forAllValid $ \(layer :: x) ->
                forAllValid $ \(grad :: Gradient x) ->
                    shouldBeValid $ runUpdate lParams layer grad
    describeWith "norm" $
        it "creates valid output" $ forAllValid $ shouldBeValid . norm @x
    describeWith "distance" $
        it "creates valid output" $
        forAllValid $ \(layer :: x) ->
            forAllValid $ \(layer' :: x) ->
                shouldBeValid $ distance layer layer'
  where
    layerName = show . typeRep $ Proxy @x
    typeLabels = unwords [" for layer ", layerName]
    describeWith :: String -> SpecWith a -> SpecWith a
    describeWith s = describe $ s ++ typeLabels
