{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Grenade.QuickCheck.Layers.FullyConnected.Gen where

import Grenade

import GHC.TypeLits (KnownNat)

import Data.GenValidity

import Numeric.LinearAlgebra.Static

instance (KnownNat i, KnownNat o) => GenUnchecked (FullyConnected' i o) where
    genUnchecked = do
        s1 <- genValid
        s2 <- genValid
        let wB = randomVector s1 Uniform * 2 - 1
            wN = uniformSample s2 (-1) 1
        pure $ FullyConnected' wB wN
    shrinkUnchecked = const []

instance (KnownNat i, KnownNat o) => GenValid (FullyConnected' i o) where
    genValid = genUnchecked

instance (KnownNat i, KnownNat o) => GenUnchecked (FullyConnected i o) where
    shrinkUnchecked = const []

instance (KnownNat i, KnownNat o) => GenValid (FullyConnected i o) where
    genValid = genUnchecked
