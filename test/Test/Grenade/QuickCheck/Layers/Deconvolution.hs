{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.Grenade.QuickCheck.Layers.Deconvolution where

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
         , KnownNat ((kernelRows * kernelColumns) * filters)
         ) =>
         GenUnchecked (Deconvolution channels filters kernelRows kernelColumns strideRows strideColumns) where
    genUnchecked = Deconvolution <$> genUnchecked <*> genUnchecked
    shrinkUnchecked = const []

instance ( KnownNat channels
         , KnownNat filters
         , KnownNat kernelRows
         , KnownNat kernelColumns
         , KnownNat strideRows
         , KnownNat strideColumns
         , KnownNat ((kernelRows * kernelColumns) * filters)
         ) =>
         GenValid (Deconvolution channels filters kernelRows kernelColumns strideRows strideColumns) where
    genValid = Deconvolution <$> genValid <*> genValid
