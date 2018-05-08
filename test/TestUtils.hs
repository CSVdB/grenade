{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module TestUtils where

import Grenade

import GHC.TypeLits

import Test.Hspec
import Test.Hspec.Runner

toTests :: Spec -> IO Bool
toTests spec = (== 0) . summaryFailures <$> hspecResult spec

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
