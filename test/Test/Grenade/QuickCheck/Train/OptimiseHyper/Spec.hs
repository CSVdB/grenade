module Test.Grenade.QuickCheck.Train.OptimiseHyper.Spec
    ( tests
    ) where

import Grenade.Train.OptimiseHyper.Internal

import TestUtils

import Test.Grenade.QuickCheck.Gen
import Test.Grenade.QuickCheck.Train.OptimiseHyper.Gen ()
import Test.QuickCheck

import Test.Hspec
import Test.Validity

tests :: IO Bool
tests = toTests spec

posIntGen :: Gen Int
posIntGen = genValid `suchThat` (>= 0)

spec :: Spec
spec = do
    describe "nextFu" $
        it "creates valid values" $
        forAll genValid $ \fu -> validate (nextFu fu) `shouldBe` valid
    describe "changeParams" $
        it "creates valid values" $
        forAll genValid $ \fu ->
            forAll genValid $ \hyperParams ->
                forAll genValid $ \posDouble ->
                    validate (changeParams fu hyperParams posDouble) `shouldBe`
                    valid
--    describe "genParamSets" $
--        it "creates valid values" $
--        forAll genValid $ \fu ->
--            forAll genValid $ \posDouble ->
--                forAll genValid $ \hyperParams -> do
--                    newHyperParams <- genParamSets fu posDouble hyperParams
--                    validate newHyperParams `shouldBe` valid
--    describe "updateHyperParams" $
--        it "creates valid values" $
--        forAll genValid $ \epochs ->
--            forAll genValid $ \updateFactor ->
--                forAll genValid $ \net ->
--                    forAll genValid $ \trainSet ->
--                        forAll genValid $ \valSet ->
--                            forAll genValid $ \fu ->
--                                forAll genValid $ \params ->
--                                    forAll genValid $ \alpha -> do
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
