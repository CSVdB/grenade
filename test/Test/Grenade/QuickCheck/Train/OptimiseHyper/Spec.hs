{-# LANGUAGE TypeApplications #-}

module Test.Grenade.QuickCheck.Train.OptimiseHyper.Spec
    ( tests
    ) where

import Grenade.Train.OptimiseHyper.Internal

import TestUtils

import Control.Monad.Random

import Test.Grenade.QuickCheck.Layers.Gen ()
import Test.Grenade.QuickCheck.Train.OptimiseHyper.Gen ()

import Test.Hspec

--import Test.QuickCheck
import Test.Validity

tests :: IO Bool
tests = toTests spec

spec :: Spec
spec = do
    describe "nextFu" $
        it "creates valid values" $
        forAllValid $ \fu -> shouldBeValid $ nextFu fu
    describe "changeParams" $
        it "creates valid values" $
        forAllValid $ \fu ->
            forAllValid $ \hyperParams ->
                forAllValid $ \posDouble ->
                    shouldBeValid $ changeParams fu hyperParams posDouble
    describe "genParams" $
        it "creates valid values" $
        forAllValid $ \seed ->
            forAllValid $ \fu ->
                forAllValid $ \posDouble ->
                    forAllValid $ \hyperParams ->
                        shouldBeValid $
                        evalRand (genParams fu posDouble hyperParams) $
                        mkStdGen seed
    describe "updateRegulariser" $
        it "creates valid values" $
        forAllValid $ \paramInfo ->
            forAllValid $ \alphaExp ->
                shouldBeValid $ updateRegulariser paramInfo alphaExp
--    describe "updateHyperParams" $
--        it "creates valid values" $
--        forAllValid $ \seed ->
--            forAll posIntGen $ \epochs ->
--                forAllValid $ \uf ->
--                    forAllValid @NN $ \net ->
--                        forAllValid $ \train ->
--                            forAllValid $ \val ->
--                                forAllValid $ \fu ->
--                                    forAllValid $ \params ->
--                                        forAllValid $ \alphaExp ->
--                                            shouldBeValid $
--                                            evalRand
--                                                (updateHyperParams
--                                                     epochs
--                                                     uf
--                                                     net
--                                                     train
--                                                     val
--                                                     fu
--                                                     params
--                                                     alphaExp) $
--                                            mkStdGen seed
