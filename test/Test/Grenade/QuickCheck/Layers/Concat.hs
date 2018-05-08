{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Test.Grenade.QuickCheck.Layers.Concat where

import Grenade

import Data.GenValidity

instance (GenUnchecked x, GenUnchecked y) =>
         GenUnchecked (Concat (m :: Shape) x (n :: Shape) y) where
    genUnchecked = Concat <$> genUnchecked <*> genUnchecked

instance (GenValid x, GenValid y) =>
         GenValid (Concat (m :: Shape) x (n :: Shape) y) where
    genValid = Concat <$> genValid <*> genValid
