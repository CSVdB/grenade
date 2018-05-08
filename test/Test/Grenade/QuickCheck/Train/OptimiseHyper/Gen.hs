{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.Grenade.QuickCheck.Train.OptimiseHyper.Gen where

import Grenade
import Grenade.Train.OptimiseHyper.Internal

import Test.Grenade.QuickCheck.Gen ()

import Test.QuickCheck
import Test.Validity

import GHC.TypeLits

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

instance (KnownNat m, KnownNat n) =>
         GenUnchecked (TrainInfo ('D1 n) ('D1 m))

instance (KnownNat i, KnownNat j, KnownNat n) =>
         GenUnchecked (TrainInfo ('D1 n) ('D2 i j))

instance (KnownNat i, KnownNat j, KnownNat k, KnownNat (i * k), KnownNat n) =>
         GenUnchecked (TrainInfo ('D1 n) ('D3 i j k))

instance (KnownNat m, KnownNat a, KnownNat b) =>
         GenUnchecked (TrainInfo ('D2 a b) ('D1 m))

instance (KnownNat i, KnownNat j, KnownNat a, KnownNat b) =>
         GenUnchecked (TrainInfo ('D2 a b) ('D2 i j))

instance ( KnownNat i
         , KnownNat j
         , KnownNat k
         , KnownNat (i * k)
         , KnownNat a
         , KnownNat b
         ) =>
         GenUnchecked (TrainInfo ('D2 a b) ('D3 i j k))

instance (KnownNat m, KnownNat a, KnownNat b, KnownNat c, KnownNat (a * c)) =>
         GenUnchecked (TrainInfo ('D3 a b c) ('D1 m))

instance ( KnownNat i
         , KnownNat j
         , KnownNat a
         , KnownNat b
         , KnownNat c
         , KnownNat (a * c)
         ) =>
         GenUnchecked (TrainInfo ('D3 a b c) ('D2 i j))

instance ( KnownNat i
         , KnownNat j
         , KnownNat k
         , KnownNat a
         , KnownNat b
         , KnownNat c
         , KnownNat (i * k)
         , KnownNat (a * c)
         ) =>
         GenUnchecked (TrainInfo ('D3 a b c) ('D3 i j k))
