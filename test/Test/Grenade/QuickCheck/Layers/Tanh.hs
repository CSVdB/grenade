{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Test.Grenade.QuickCheck.Layers.Tanh where

import Grenade

import Data.GenValidity

instance GenUnchecked Tanh where
    genUnchecked = pure Tanh

instance GenValid Tanh where
    genValid = genUnchecked
