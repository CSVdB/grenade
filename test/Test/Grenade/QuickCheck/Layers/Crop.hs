{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Test.Grenade.QuickCheck.Layers.Crop where

import Grenade

import Data.GenValidity

import GHC.TypeLits

instance (KnownNat l, KnownNat t, KnownNat r, KnownNat b) =>
         GenUnchecked (Crop l t r b) where
    genUnchecked = pure Crop

instance (KnownNat l, KnownNat t, KnownNat r, KnownNat b) =>
         GenValid (Crop l t r b) where
    genValid = genUnchecked
