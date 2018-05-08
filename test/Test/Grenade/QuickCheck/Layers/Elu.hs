{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Test.Grenade.QuickCheck.Layers.Elu where

import Grenade

import Data.GenValidity

instance GenUnchecked Elu where
    genUnchecked = pure Elu

instance GenValid Elu where
    genValid = genUnchecked
