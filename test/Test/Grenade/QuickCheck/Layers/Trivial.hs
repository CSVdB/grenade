{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Test.Grenade.QuickCheck.Layers.Trivial where

import Grenade

import Data.GenValidity

instance GenUnchecked Trivial where
    genUnchecked = pure Trivial

instance GenValid Trivial where
    genValid = genUnchecked
