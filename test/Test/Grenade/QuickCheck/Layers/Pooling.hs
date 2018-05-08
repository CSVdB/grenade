{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Test.Grenade.QuickCheck.Layers.Pooling where

import Grenade

import GHC.TypeLits

import Data.GenValidity

instance (KnownNat k, KnownNat k', KnownNat s, KnownNat s') =>
         GenUnchecked (Pooling k k' s s') where
    genUnchecked = pure Pooling

instance (KnownNat k, KnownNat k', KnownNat s, KnownNat s') =>
         GenValid (Pooling k k' s s') where
    genValid = genUnchecked
