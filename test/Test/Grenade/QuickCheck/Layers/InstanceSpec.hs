{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Grenade.QuickCheck.Layers.InstanceSpec
    ( layerGenValidSpec
    , tests
    ) where

import Grenade

import Test.Hspec
import TestUtils

import Test.Grenade.QuickCheck.Layers.Gen ()
import Test.Validity
import Type.Reflection

tests :: IO Bool
tests = toTests spec

spec :: Spec
spec = do
    layerGenValidSpec @Logit
    layerGenValidSpec @Reshape
    layerGenValidSpec @FCL
    layerGenValidSpec @Tanh

layerGenValidSpec ::
       forall x.
       ( Typeable x
       , Typeable (Gradient x)
       , Show x
       , Show (Gradient x)
       , UpdateLayer x
       , GenValid x
       , GenValid (Gradient x)
       )
    => Spec
layerGenValidSpec = do
    genValidSpec @x
    genValidSpec @(Gradient x)
