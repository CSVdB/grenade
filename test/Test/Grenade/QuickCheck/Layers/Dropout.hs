{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Test.Grenade.QuickCheck.Layers.Dropout where

import Grenade.Layers.Dropout

import Data.GenValidity

instance GenUnchecked Dropout where
    genUnchecked = Dropout <$> genUnchecked <*> genUnchecked

instance GenValid Dropout where
    genValid = Dropout <$> genValid <*> genValid
