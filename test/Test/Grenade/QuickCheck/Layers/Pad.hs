{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Test.Grenade.QuickCheck.Layers.Pad where

import Grenade

import GHC.TypeLits

import Data.GenValidity

instance (KnownNat l, KnownNat t, KnownNat r, KnownNat p) =>
         GenUnchecked (Pad l t r p) where
    genUnchecked = pure Pad

instance (KnownNat l, KnownNat t, KnownNat r, KnownNat p) =>
         GenValid (Pad l t r p) where
    genValid = genUnchecked
