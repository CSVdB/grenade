{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Test.Grenade.QuickCheck.Layers.Sinusoid where

import Grenade

import Data.GenValidity

instance GenUnchecked Sinusoid where
    genUnchecked = pure Sinusoid

instance GenValid Sinusoid where
    genValid = genUnchecked
