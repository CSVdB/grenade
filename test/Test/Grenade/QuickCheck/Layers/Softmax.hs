{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Test.Grenade.QuickCheck.Layers.Softmax where

import Grenade

import Data.GenValidity

instance GenUnchecked Softmax where
    genUnchecked = pure Softmax

instance GenValid Softmax where
    genValid = genUnchecked
