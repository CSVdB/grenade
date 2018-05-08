{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Grenade.Layers.Sinusoid
Description : Sinusoid nonlinear layer
Copyright   : (c) Manuel Schneckenreither, 2018
License     : BSD2
Stability   : experimental
-}
module Grenade.Layers.Sinusoid
    ( Sinusoid(..)
    ) where

import Data.Serialize
import Data.Singletons
import Data.Validity

import GHC.Generics hiding (S)

import Grenade.Core
import Grenade.Utils.SumSquaredParams

-- | A Sinusoid layer.
--   A layer which can act between any shape of the same dimension, performing a sin function.
data Sinusoid =
    Sinusoid
    deriving (Show, Generic)

instance UpdateLayer Sinusoid where
    type Gradient Sinusoid = ()
    runUpdate _ _ _ = Sinusoid
    createRandom = return Sinusoid

instance Serialize Sinusoid where
    put _ = return ()
    get = return Sinusoid

instance (a ~ b, SingI a) => Layer Sinusoid a b where
    type Tape Sinusoid a b = S a
    runForwards _ a = (a, sin a)
    runBackwards _ a g = ((), cos a * g)

instance SumSquaredParams Sinusoid where
    getSumSquaredParams _layer = mempty
    getSumSquaredParamsDelta _proxy _gradient = mempty

instance Validity Sinusoid where
    validate = trivialValidation
