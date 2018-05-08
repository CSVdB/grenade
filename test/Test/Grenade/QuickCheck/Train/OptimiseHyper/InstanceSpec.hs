{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Grenade.QuickCheck.Train.OptimiseHyper.InstanceSpec
    ( tests
    ) where

import Grenade.Train.OptimiseHyper.Internal

import Test.Grenade.QuickCheck.Train.OptimiseHyper.Gen ()
import TestUtils

import Test.Hspec
import Test.Validity

tests :: IO Bool
tests = toTests spec

spec :: Spec
spec = genValidSpec @FieldToUpdate
