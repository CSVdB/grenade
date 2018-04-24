{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Grenade.Train.OptimiseHyper
    ( findHyperParams
    ) where

import Grenade.Core
import Grenade.Train.DataSet
import Grenade.Train.HyperParamInfo
import Grenade.Train.HyperParams
import Grenade.Utils.PositiveDouble
import Grenade.Utils.PositiveDouble.Internal
import Grenade.Utils.SumSquaredParams
import Grenade.Utils.Accuracy

import GHC.Generics

import Data.List.Extra (maximumOn)

import Data.Maybe (fromJust)

import Test.QuickCheck.Checkers (gens)
import Test.QuickCheck (choose)

import Data.Singletons (SingI)
import Data.Singletons.Prelude (Head, Last)

import Control.Monad.IO.Class

nOfValues :: Int
nOfValues = 5

data FieldToUpdate
    = Rate
    | Momentum
    | Regulator
    | Decay
    deriving (Show, Eq, Generic)

nextFu :: FieldToUpdate -> FieldToUpdate
nextFu Regulator = Rate
nextFu Rate = Momentum
nextFu Momentum = Decay
nextFu Decay = Regulator

changeParams :: FieldToUpdate -> HyperParams -> PositiveDouble -> HyperParams
changeParams Rate (HyperParams lparams@LearningParameters {..} decayFactor) x =
    HyperParams lparams {learningRate = pMultiply x learningRate} decayFactor
changeParams Momentum (HyperParams lparams@LearningParameters {..} decayFactor) x =
    HyperParams lparams {learningMomentum = pMultiply x learningMomentum } decayFactor
changeParams Regulator (HyperParams lparams@LearningParameters {..} decayFactor) x =
    HyperParams lparams {learningRegulariser = pMultiply x learningRegulariser } decayFactor
changeParams Decay params@(HyperParams lparams decayFactor) x =
    case HyperParams lparams <$> dMultiply x decayFactor of
        Nothing -> params
        Just newParams -> newParams

genParamSets :: MonadIO m => FieldToUpdate -> Double -> HyperParams -> m [HyperParams]
genParamSets fu updateFactor params =
    liftIO . gens nOfValues $ changeParams fu params . PositiveDouble . exp <$> choose (-updateFactor, updateFactor)

updateHyperParams ::
       forall (shapes :: [Shape]) (layers :: [*]) (i :: Shape) (o :: Shape) (m :: * -> *).
       ( SingI o
       , i ~ Head shapes
       , o ~ Last shapes
       , MonadIO m
       , SumSquaredParams (Network layers shapes)
       )
    => Int
    -> Network layers shapes
    -> DataSet i o
    -> DataSet i o
    -> FieldToUpdate
    -> HyperParams
    -> m (HyperParams, FieldToUpdate, Accuracy)
updateHyperParams epochs net trainSet valSet fu params = do
    paramSets <- genParamSets fu 2 params
    let bestParams = flip maximumOn paramSets getAccuracyFromHyperParams :: HyperParams
    let newFu = nextFu fu
    let valAcc = (getAccuracyFromHyperParams bestParams :: Accuracy)
    liftIO . putStrLn $ showAccuracy "validation" valAcc
    pure (bestParams, newFu, valAcc)
  where
    getAccuracyFromHyperParams = getAccuracy . getHyperParamInfo epochs net trainSet valSet

requiredAcc :: Accuracy
requiredAcc = fromJust $ accuracyM 0.95

findHyperParams ::
       forall (shapes :: [Shape]) (layers :: [*]) (i :: Shape) (o :: Shape) (m :: * -> *).
       ( SingI o
       , i ~ Head shapes
       , o ~ Last shapes
       , MonadIO m
       , SumSquaredParams (Network layers shapes)
       )
    => Int
    -> Network layers shapes
    -> DataSet i o
    -> DataSet i o
    -> FieldToUpdate
    -> HyperParams
    -> m (HyperParams)
findHyperParams epochs net trainSet valSet fu0 params0 = do
    (params, fu, valAcc) <- updateHyperParams epochs net trainSet valSet fu0 params0
    if valAcc > requiredAcc
        then pure params
        else findHyperParams epochs net trainSet valSet fu params
