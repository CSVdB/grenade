{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Test.Grenade.QuickCheck.Gen where

import Grenade

import Data.GenValidity
import Data.Proxy
import Data.Singletons
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
        (SV.fromList <$> replicateM i genUnchecked) `suchThatMap` create
    shrinkUnchecked = const []

instance KnownNat n => GenValid (R n) where
    genValid = do
        let i = fromInteger $ natVal (Proxy @n)
        (SV.fromList <$> replicateM i (choose (0, 1))) `suchThatMap` create

instance (KnownNat i, KnownNat j) => GenUnchecked (L i j) where
    genUnchecked = do
        let i' = fromInteger $ natVal (Proxy @i)
        let j' = fromInteger $ natVal (Proxy @j)
        (reshape j' . SV.fromList <$> replicateM (i' * j') genUnchecked :: Gen (Matrix Double)) `suchThatMap`
            create
    shrinkUnchecked = const []

instance (KnownNat i, KnownNat j) => GenValid (L i j) where
    genValid = do
        let i' = fromInteger $ natVal (Proxy @i)
        let j' = fromInteger $ natVal (Proxy @j)
        (reshape j' . SV.fromList <$> replicateM (i' * j') (choose (0, 1)) :: Gen (Matrix Double)) `suchThatMap`
            create

instance SingI x => GenUnchecked (S x) where
    genUnchecked =
        case sing :: Sing x of
            D1Sing SNat -> S1D <$> genUnchecked
            D2Sing SNat SNat -> S2D <$> genUnchecked
            D3Sing SNat SNat SNat -> S3D <$> genUnchecked
    shrinkUnchecked = const []

instance SingI x => GenValid (S x) where
    genValid =
        case sing :: Sing x of
            D1Sing SNat -> S1D <$> genValid
            D2Sing SNat SNat -> S2D <$> genValid
            D3Sing SNat SNat SNat -> S3D <$> genValid
