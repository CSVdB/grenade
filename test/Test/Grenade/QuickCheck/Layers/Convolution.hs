{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.Grenade.QuickCheck.Layers.Convolution where

import Grenade

import GHC.TypeLits

import Data.GenValidity

import Test.Grenade.QuickCheck.Gen ()

instance ( KnownNat channels
         , KnownNat filters
         , KnownNat kernelRows
         , KnownNat kernelColumns
         , KnownNat strideRows
         , KnownNat strideColumns
         , KnownNat (kernelRows * kernelColumns * channels)
         ) =>
         GenUnchecked (Convolution channels filters kernelRows kernelColumns strideRows strideColumns) where
    genUnchecked = Convolution <$> genUnchecked <*> genUnchecked
    shrinkUnchecked = const []

instance ( KnownNat channels
         , KnownNat filters
         , KnownNat kernelRows
         , KnownNat kernelColumns
         , KnownNat strideRows
         , KnownNat strideColumns
         , KnownNat (kernelRows * kernelColumns * channels)
         ) =>
         GenValid (Convolution channels filters kernelRows kernelColumns strideRows strideColumns) where
    genValid = Convolution <$> genValid <*> genValid

instance ( KnownNat channels
         , KnownNat filters
         , KnownNat kernelRows
         , KnownNat kernelColumns
         , KnownNat strideRows
         , KnownNat strideColumns
         , KnownNat (kernelRows * kernelColumns * channels)
         ) =>
         GenUnchecked (Convolution' channels filters kernelRows kernelColumns strideRows strideColumns) where
    genUnchecked = Convolution' <$> genUnchecked
    shrinkUnchecked = const []

instance ( KnownNat channels
         , KnownNat filters
         , KnownNat kernelRows
         , KnownNat kernelColumns
         , KnownNat strideRows
         , KnownNat strideColumns
         , KnownNat (kernelRows * kernelColumns * channels)
         ) =>
         GenValid (Convolution' channels filters kernelRows kernelColumns strideRows strideColumns) where
    genValid = Convolution' <$> genValid
