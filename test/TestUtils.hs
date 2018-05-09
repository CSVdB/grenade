{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module TestUtils where

import Grenade

import GHC.TypeLits

import Test.Hspec
import Test.Hspec.Runner

import Data.Validity

toTests :: Spec -> IO Bool
toTests spec = (== 0) . summaryFailures <$> hspecResult spec

testValidity :: Validity a => a -> Expectation
testValidity x =
    case prettyValidation x of
        Right _ -> pure ()
        Left errMess -> expectationFailure errMess

type Size = 28

type Image = 'D2 Size Size

type Output = 'D1 O

type I = Size * Size

type O = 10

type M = 30

type Ishape = 'D1 I

type Oshape = 'D1 O

type Mshape = 'D1 M

type SI = S Ishape

type SO = S Oshape

type SM = S Mshape

type FCL = FullyConnected I M

type FCLTape = Tape FCL Ishape Mshape

type Grad = Gradient FCL

type NN
     = Network '[ Reshape, FCL, Tanh, FullyConnected M O, Logit] '[ Image, Ishape, Mshape, Mshape, Oshape, Oshape]

type M2 = 2 * M

type M2shape = 'D1 M2

type ConvKernelSize = 5

type ConvStride = 3

type ConvInptShape = 'D2 29 29

type ConvOutptShape = 'D2 9 9

type CropLeft = 2

type CropTop = 2

type CropRight = 2

type CropBottom = 2

type CropHorInput = 20

type CropVertInput = 15

type CropHorOutput = CropHorInput - CropLeft - CropRight

type CropVertOutput = CropVertInput - CropTop - CropBottom

type CropInputShape = 'D2 CropHorInput CropVertInput

type CropOutputShape = 'D2 CropHorOutput CropVertOutput
