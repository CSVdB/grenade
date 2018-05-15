{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Grenade.Utils.MetricNormedSpace where

import Numeric.LinearAlgebra (flatten, norm_1)
import Numeric.LinearAlgebra.Static

import GHC.TypeLits

import Grenade.Utils.PositiveDouble.Internal

class MetricNormedSpace x where
    zeroM :: x
    distance :: x -> x -> PositiveDouble

norm :: MetricNormedSpace x => x -> PositiveDouble
norm = distance zeroM

instance KnownNat n => MetricNormedSpace (R n) where
    zeroM = konst 0
    distance v v' = PositiveDouble . norm_1 . extract $ v - v'

instance (KnownNat i, KnownNat j) => MetricNormedSpace (L i j) where
    zeroM = konst 0
    distance m m' =
        PositiveDouble . norm_1 $ flatten (extract m) - flatten (extract m')
