{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Test.Grenade.QuickCheck.Layers.Convolution where

import Grenade

import Data.GenValidity

instance GenUnchecked (Convolution c f k k' s s') where
    genUnchecked = Convolution <$> genUnchecked <*> genUnchecked

instance GenValid (convolutin c f k k' s s') where
    genValid = Convolution <$> genValid <*> genValid
