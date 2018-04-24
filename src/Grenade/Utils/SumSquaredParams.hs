{-# LANGUAGE FlexibleContexts #-}

module Grenade.Utils.SumSquaredParams where

import Grenade.Core.Layer
import Grenade.Utils.PositiveDouble

import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.Static as LA

import qualified Data.Vector.Storable as VS

import Control.Monad.Catch

-- This is a typeclass to calculate sum w^2 and sum (delta w)^2
-- where w are the parameters of a layer, and delta w of the gradient.
-- This is used to automate optimising the learning parameters.
class UpdateLayer layer => SumSquaredParams layer where
    getSumSquaredParams :: layer -> PositiveDouble
    getSumSquaredParamsDelta :: proxy layer -> Gradient layer -> PositiveDouble

sumSquaredParamsFromMatrix :: LA.Sized Double s LA.Matrix => s -> PositiveDouble
sumSquaredParamsFromMatrix m =
    case constructPositiveDouble . VS.sum . VS.map (** 2) . LA.flatten $ LA.extract m of
        Right w -> w
        Left e -> error $ displayException e

sumSquaredParamsFromVector :: LA.Sized Double s LA.Vector => s -> PositiveDouble
sumSquaredParamsFromVector v =
    case constructPositiveDouble . VS.sum . VS.map (** 2) $ LA.extract v of
        Right w -> w
        Left e -> error $ displayException e
