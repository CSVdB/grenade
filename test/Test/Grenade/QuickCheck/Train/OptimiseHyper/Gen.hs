{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Test.Grenade.QuickCheck.Train.OptimiseHyper.Gen where

import Grenade.Train.OptimiseHyper.Internal

import Test.Grenade.QuickCheck.Gen ()

import Test.QuickCheck
import Test.Validity

import Data.Singletons

import GHC.Natural

instance GenUnchecked FieldToUpdate where
    genUnchecked = elements [Rate, Momentum, Decay]

instance GenValid FieldToUpdate where
    genValid = genUnchecked

instance GenUnchecked Natural where
    genUnchecked = arbitrarySizedNatural
    shrinkUnchecked = const []

instance GenValid Natural where
    genValid = genUnchecked

instance (SingI x, SingI y) => GenUnchecked (TrainInfo x y)

instance (SingI x, SingI y) => GenValid (TrainInfo x y)
