{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Test.Grenade.QuickCheck.Layers.Reshape where

import Grenade

import Data.GenValidity

instance GenUnchecked Reshape where
    genUnchecked = pure Reshape

instance GenValid Reshape where
    genValid = genUnchecked
