{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.Grenade.QuickCheck.Network where

import TestUtils

import Grenade

import Data.GenValidity
import Data.Singletons

instance SingI i => GenUnchecked (Network '[] '[ i]) where
    genUnchecked = pure NNil
    shrinkUnchecked = const []

instance SingI i => GenValid (Network '[] '[ i]) where
    genValid = genUnchecked

instance ( SingI i
         , SingI h
         , Layer x i h
         , GenUnchecked x
         , GenUnchecked (Network xs (h ': hs))
         ) =>
         GenUnchecked (Network (x ': xs) (i ': (h ': hs))) where
    genUnchecked = (:~>) <$> genUnchecked <*> genUnchecked
    shrinkUnchecked = const []

instance ( SingI i
         , SingI h
         , Layer x i h
         , GenValid x
         , GenValid (Network xs (h ': hs))
         ) =>
         GenValid (Network (x ': xs) (i ': (h ': hs))) where
    genValid = (:~>) <$> genValid <*> genValid
