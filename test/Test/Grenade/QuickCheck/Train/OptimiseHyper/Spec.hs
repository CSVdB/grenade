{-# LANGUAGE TypeApplications #-}

module Test.Grenade.QuickCheck.Train.OptimiseHyper.Spec
    ( tests
    ) where

import Grenade.Train.OptimiseHyper.Internal

import TestUtils

import Test.Grenade.QuickCheck.Gen
import Test.Grenade.QuickCheck.Train.OptimiseHyper.Gen ()

import Test.Hspec
import Test.Validity

tests :: IO Bool
tests = toTests spec

-- posIntGen :: Gen Int
-- posIntGen = genValid `suchThat` (>= 0)
spec :: Spec
spec = do
    describe "nextFu" $
        it "creates valid values" $
        forAllValid $ \fu -> validate (nextFu fu) `shouldBe` valid
    describe "changeParams" $
        it "creates valid values" $
        forAllValid $ \fu ->
            forAllValid $ \hyperParams ->
                forAllValid $ \posDouble ->
                    testValidity $ changeParams fu hyperParams posDouble
    describe "genParamSets" $
        it "creates valid values" $
        forAllValid $ \fu ->
            forAllValid $ \posDouble ->
                forAllValid $ \hyperParams ->
                    testValidity =<< genParamSets fu posDouble hyperParams
--    describe "updateHyperParams" $
--        it "creates valid values" $
--        forAllValid $ \epochs ->
--            forAllValid $ \updateFactor ->
--                forAll (genValid @NN) $ \net ->
--                    forAllValid $ \trainSet ->
--                        forAllValid $ \valSet ->
--                            forAllValid $ \fu ->
--                                forAllValid $ \params ->
--                                    forAllValid $ \alpha -> do
--                                        result <-
--                                            updateHyperParams
--                                                epochs
--                                                updateFactor
--                                                net
--                                                trainSet
--                                                valSet
--                                                fu
--                                                params
--                                                alpha
--                                        validate result `shouldBe` valid
