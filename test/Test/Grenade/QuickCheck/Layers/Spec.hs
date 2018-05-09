{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Test.Grenade.QuickCheck.Layers.Spec
    ( tests
    ) where

import TestUtils

import Test.Grenade.QuickCheck.Layers.Gen ()
import Test.Grenade.QuickCheck.Layers.LayerSpec

import Grenade
import Grenade.Layers.Softmax (softmax)

import Numeric.LinearAlgebra.Static (R)
import Test.Hspec
import Test.Validity

tests :: IO Bool
tests = toTests spec

spec :: Spec
spec = do
    describe "softmax" $
        it "generates valid values" $
        forAllValid $ \(v :: R O) -> testValidity (softmax v)
    layerSpec
        @(Concat Mshape (FullyConnected I M) Mshape (FullyConnected I M))
        @Ishape
        @M2shape
    layerSpec
        @(Convolution 1 1 ConvKernelSize ConvKernelSize ConvStride ConvStride)
        @ConvInptShape
        @ConvOutptShape
    layerSpec
        @(Crop CropLeft CropTop CropRight CropBottom)
        @CropInputShape
        @CropOutputShape
    layerSpec
        @(Deconvolution 1 1 ConvKernelSize ConvKernelSize ConvStride ConvStride)
        @ConvOutptShape
        @ConvInptShape
    layerSpec @Dropout @Ishape @Ishape
    layerSpec @Elu @Ishape @Ishape
    layerSpec @(FullyConnected I M) @Ishape @Mshape
    layerSpec @Logit @Ishape @Ishape
    layerSpec @(Merge (FullyConnected I M) (FullyConnected I M)) @Ishape @Mshape
    layerSpec
        @(Pad CropLeft CropTop CropRight CropBottom)
        @CropOutputShape
        @CropInputShape
    layerSpec
        @(Pooling ConvKernelSize ConvKernelSize ConvStride ConvStride)
        @ConvInptShape
        @ConvOutptShape
    layerSpec @Relu @Ishape @Ishape
    layerSpec @Reshape @Image @Ishape
    layerSpec @Sinusoid @Image @Image
    layerSpec @Softmax @Oshape @Oshape
    layerSpec @Tanh @Image @Image
    layerSpec @Trivial @Image @Image
