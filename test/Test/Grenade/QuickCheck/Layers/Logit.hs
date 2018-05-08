{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Test.Grenade.QuickCheck.Layers.Logit where

import Grenade

import Data.GenValidity

instance GenUnchecked Logit where
    genUnchecked = pure Logit

instance GenValid Logit where
    genValid = genUnchecked
