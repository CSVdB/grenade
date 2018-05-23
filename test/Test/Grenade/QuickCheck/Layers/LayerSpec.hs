{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Test.Grenade.QuickCheck.Layers.LayerSpec
    ( layerSpec
    ) where

import Grenade

import Control.Monad.Random
import Data.GenValidity
import Data.Proxy
import Data.Singletons
import Data.Typeable

import Test.Hspec
import Test.Validity

import Test.Grenade.QuickCheck.Layers.Gen ()

layerSpec ::
       forall x (i :: Shape) (o :: Shape).
       ( Layer x i o
       , SingI i
       , SingI o
       , GenValid x
       , GenValid (Gradient x)
       , GenValid (Tape x i o)
       , Show x
       , Show (Gradient x)
       , Show (Tape x i o)
       , Validity (Tape x i o)
       , MetricNormedSpace x
       , Typeable x
       , Typeable (Gradient x)
       , Typeable i
       , Typeable o
       )
    => Spec
layerSpec =
    describe layerName $ do
        genValidSpec @x
        genValidSpec @(Gradient x)
        describe
            (concat ["createRandom :: MonadRandom m => m ", withBrackets xName]) $
            it (concat ["creates valid \'", xName, "\'s"]) $
            forAllValid $ \seed ->
                shouldBeValid $ evalRand (createRandom @x) $ mkStdGen seed
        describe
            (unwords
                 [ "runForwards ::"
                 , xName
                 , "->"
                 , iShapeName
                 , "->"
                 , '(' : tapeName ++ ","
                 , oShapeName ++ ")"
                 ]) $
            it "creates valids on valids" $
            forAllValid $ \(inpt :: S i) ->
                forAllValid $ \(layer :: x) ->
                    shouldBeValid (runForwards layer inpt :: (Tape x i o, S o))
        describe
            (unwords
                 [ "runBackwards ::"
                 , xName
                 , "->"
                 , tapeName
                 , "->"
                 , oShapeName
                 , "->"
                 , '(' : gradName ++ ","
                 , iShapeName ++ ")"
                 ]) $
            it "creates valids on valids" $
            forAllValid $ \(tape :: Tape x i o) ->
                forAllValid $ \(layer :: x) ->
                    forAllValid $ \(outpt :: S o) ->
                        shouldBeValid
                            (runBackwards layer tape outpt :: (Gradient x, S i))
        describe
            (unwords
                 [ "runUpdate :: LearningParameters ->"
                 , xName
                 , "->"
                 , gradName
                 , "->"
                 , xName
                 ]) $
            it "creates valids on valids" $
            forAllValid $ \(lParams :: LearningParameters) ->
                forAllValid $ \(layer :: x) ->
                    forAllValid $ \(grad :: Gradient x) ->
                        shouldBeValid $ runUpdate lParams layer grad
        describeWith (unwords ["norm ::", xName, "->", "PositiveDouble"]) $
            it "creates valids on valids" $
            forAllValid $ shouldBeValid . norm @x
        describeWith
            (unwords ["distance ::", xName, "->", xName, "->", "PositiveDouble"]) $
            it "creates valids on valids" $
            forAllValid $ \(layer :: x) ->
                forAllValid $ \(layer' :: x) ->
                    shouldBeValid $ distance layer layer'
  where
    xName = show . typeRep $ Proxy @x
    iName = show . typeRep $ Proxy @i
    oName = show . typeRep $ Proxy @o
    withBrackets name =
        case words name of
            [_] -> name
            _ -> '(' : name ++ ")"
    toShapeName name = "S " ++ withBrackets name
    iShapeName = toShapeName iName
    oShapeName = toShapeName oName
    layerName =
        unwords
            [ "Layer"
            , withBrackets xName
            , withBrackets iName
            , withBrackets oName
            ]
    tapeName = unwords ["Tape", xName, iName, oName]
    gradName = unwords ["Gradient", withBrackets xName]
    typeLabels = unwords [" for layer ", layerName]
    describeWith :: String -> SpecWith a -> SpecWith a
    describeWith s = describe $ s ++ typeLabels
