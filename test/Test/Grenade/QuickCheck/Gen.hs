{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Test.Grenade.QuickCheck.Gen where

import Grenade

import Data.GenValidity
import Data.Proxy
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as SV

import Control.Monad

import GHC.TypeLits

import Test.QuickCheck

import Test.Grenade.QuickCheck.Network ()
import Test.Grenade.QuickCheck.Utils.Gen ()

import Numeric.LinearAlgebra.Data hiding (R)
import Numeric.LinearAlgebra.Static

instance KnownNat n => GenUnchecked (R n) where
    genUnchecked = do
        let i = fromInteger $ natVal (Proxy @n)
        v <- (SV.fromList <$> replicateM i genUnchecked :: Gen (Vector Double))
        case create v :: Maybe (R n) of
            Nothing -> genUnchecked
            Just rn -> pure rn
    shrinkUnchecked = const []

instance KnownNat n => GenValid (R n) where
    genValid = do
        let i = fromInteger $ natVal (Proxy @n)
        v <- (SV.fromList <$> replicateM i genValid :: Gen (Vector Double))
        case create v :: Maybe (R n) of
            Nothing -> genUnchecked
            Just rn -> pure rn

instance (KnownNat i, KnownNat j) => GenUnchecked (L i j) where
    genUnchecked = do
        let i' = fromInteger $ natVal (Proxy @i)
        let j' = fromInteger $ natVal (Proxy @j)
        v <-
            (reshape j' . SV.fromList <$> replicateM (i' * j') genUnchecked :: Gen (Matrix Double))
        case create v :: Maybe (L i j) of
            Nothing -> genUnchecked
            Just rn -> pure rn
    shrinkUnchecked = const []

instance (KnownNat i, KnownNat j) => GenValid (L i j) where
    genValid = do
        let i' = fromInteger $ natVal (Proxy @i)
        let j' = fromInteger $ natVal (Proxy @j)
        v <-
            (reshape j' . SV.fromList <$> replicateM (i' * j') genValid :: Gen (Matrix Double))
        case create v :: Maybe (L i j) of
            Nothing -> genUnchecked
            Just rn -> pure rn

instance KnownNat n => GenUnchecked (S ('D1 n)) where
    genUnchecked = S1D <$> genUnchecked
    shrinkUnchecked = const []

instance KnownNat n => GenValid (S ('D1 n)) where
    genValid = S1D <$> genValid

instance (KnownNat i, KnownNat j) => GenUnchecked (S ('D2 i j)) where
    genUnchecked = S2D <$> genUnchecked
    shrinkUnchecked = const []

instance (KnownNat i, KnownNat j) => GenValid (S ('D2 i j)) where
    genValid = S2D <$> genValid

instance (KnownNat i, KnownNat j, KnownNat k, KnownNat (i * k)) =>
         GenUnchecked (S ('D3 i j k)) where
    genUnchecked = S3D <$> genUnchecked @(L (i * k) j)
    shrinkUnchecked = const []

instance (KnownNat i, KnownNat j, KnownNat k, KnownNat (i * k)) =>
         GenValid (S ('D3 i j k)) where
    genValid = S3D <$> genValid @(L (i * k) j)
