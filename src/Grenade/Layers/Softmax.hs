{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Grenade.Core.Softmax
Description : Softmax loss layer
Copyright   : (c) Huw Campbell, 2016-2017
License     : BSD2
Stability   : experimental
-}
module Grenade.Layers.Softmax
    ( Softmax(..)
    , softmax
    , softmax'
    ) where

import Data.Proxy
import Data.Serialize
import Data.Validity

import GHC.Generics hiding (S)
import GHC.TypeLits
import Grenade.Core

import qualified Data.Vector.Storable as SV

import Numeric.LinearAlgebra.Static as LAS

-- | A Softmax layer
--
--   This layer is like a logit layer, but normalises
--   a set of matricies to be probabilities.
--
--   One can use this layer as the last layer in a network
--   if they need normalised probabilities.
data Softmax =
    Softmax
    deriving (Show, Generic)

instance UpdateLayer Softmax where
    type Gradient Softmax = ()
    runUpdate _ _ _ = Softmax
    createRandom = return Softmax

instance (KnownNat i) => Layer Softmax ('D1 i) ('D1 i) where
    type Tape Softmax ('D1 i) ('D1 i) = S ('D1 i)
    runForwards _ (S1D y) = (S1D y, S1D (softmax y))
    runBackwards _ (S1D y) (S1D dEdy) = ((), S1D (softmax' y dEdy))

instance Serialize Softmax where
    put _ = return ()
    get = return Softmax

-- If exp x = Infinity for a value, softmax v returns a basis vector e_n with n = maxIndex v.
-- If all values are so small that exp x = 0, the uniform distribution is returned.
softmax ::
       forall i. KnownNat i
    => LAS.R i
    -> LAS.R i
softmax xs
    | natVal (Proxy :: Proxy i) == 0 = xs
    | otherwise =
        let xs' = LAS.dvmap exp xs
            s = LAS.dot xs' 1
            n = fromIntegral $ natVal (Proxy :: Proxy i)
         in if s == 0
                then konst $ 1 / fromIntegral n
                else case constructValid $ LAS.dvmap (/ s) xs' of
                         Just v -> v
                         Nothing ->
                             let m = SV.maxIndex $ extract xs
                              in fromList $ fmap (oneOnEq m) [1 .. n]
  where
    oneOnEq x y =
        if x == y
            then 1
            else 0

softmax' :: KnownNat i => LAS.R i -> LAS.R i -> LAS.R i
softmax' x grad =
    let yTy = outer sm sm
        d = diag sm
        g = d - yTy
     in g #> grad
  where
    sm = softmax x

instance MetricNormedSpace Softmax where
    zeroM = Softmax
    distance _ _ = mempty

instance Validity Softmax where
    validate = trivialValidation
