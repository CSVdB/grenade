{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Test.Grenade.QuickCheck.Layers.FullyConnected.Gen where

import Grenade

import Test.QuickCheck hiding (vector)

import Control.Monad
import Data.GenValidity
import Data.Proxy
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as SV
import GHC.TypeLits (KnownNat, natVal)

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

instance KnownNat n => GenUnchecked (R n) where
    genUnchecked = do
        let i = fromInteger $ natVal (Proxy @n)
        v <- (SV.fromList <$> replicateM i genUnchecked :: Gen (Vector Double))
        case create v :: Maybe (R n) of
            Nothing -> genUnchecked
            Just rn -> pure rn
    shrinkUnchecked = const []

instance KnownNat n => GenValid (R n) where
    genValid = genUnchecked
--instance KnownNat n => GenUnchecked (S ('D1 n)) where
--    genUnchecked = S1D <$> genUnchecked
--    shrinkUnchecked = const []
--
--instance (KnownNat i, j) => GenUnchecked (S ('D2 i j)) where
--    genUnchecked = S2D <$> genUnchecked
--    shrinkUnchecked = const []
--
--instance GenValid (S (i :: Shape)) where
--    genValid = genUnchecked
