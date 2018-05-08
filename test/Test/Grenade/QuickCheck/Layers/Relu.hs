{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Test.Grenade.QuickCheck.Layers.Relu where

import Grenade

import Data.GenValidity

instance GenUnchecked Relu where
    genUnchecked = pure Relu

instance GenValid Relu where
    genValid = genUnchecked
