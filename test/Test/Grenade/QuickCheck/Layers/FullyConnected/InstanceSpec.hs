{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Grenade.QuickCheck.Layers.FullyConnected.InstanceSpec
    ( tests
    ) where

import Test.Hspec

import Grenade

import Test.Grenade.QuickCheck.Layers.FullyConnected.Gen ()
import Test.Hspec.Runner
import Test.Validity

type I = 10

type O = 28

tests :: IO Bool
tests = do
    summary <- hspecResult spec
    case summaryFailures summary of
        0 -> pure True
        _ -> pure False

spec :: Spec
spec = do
    genValidSpec @(FullyConnected' I O)
    genValidSpec @(FullyConnected I O)
