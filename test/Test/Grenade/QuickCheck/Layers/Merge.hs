{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Test.Grenade.QuickCheck.Layers.Merge where

import Grenade

import Data.GenValidity

instance (GenUnchecked x, GenUnchecked y) => GenUnchecked (Merge x y) where
    genUnchecked = Merge <$> genUnchecked <*> genUnchecked

instance (GenValid x, GenValid y) => GenValid (Merge x y) where
    genValid = Merge <$> genValid <*> genValid
