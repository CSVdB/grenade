{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Grenade.Train.OptimiseHyper
    ( findHyperParams
    , findHyperParamsWithSeveralRuns
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

import Test.QuickCheck.Checkers (gens)
import Test.QuickCheck (choose)

import Data.Singletons (SingI)
import Data.Singletons.Prelude (Head, Last)

import Control.Monad.IO.Class

import Data.Time.Clock

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
    -> Double
    -> Network layers shapes
    -> DataSet i o
    -> DataSet i o
    -> FieldToUpdate
    -> HyperParams
    -> m (HyperParams, FieldToUpdate, Accuracy)
updateHyperParams epochs updateFactor net trainSet valSet fu params = do
    paramSets <- genParamSets fu updateFactor params
    let bestParams = flip maximumOn paramSets getAccuracyFromHyperParams :: HyperParams
    let newFu = nextFu fu
    let valAcc = getAccuracyFromHyperParams bestParams
    liftIO . putStrLn $ showAccuracy "validation" valAcc
    pure (bestParams, newFu, valAcc)
  where
    getAccuracyFromHyperParams = getAccuracy . getHyperParamInfo epochs net trainSet valSet

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
    -> HyperParamOptimisationInfo i o
    -> FieldToUpdate
    -> HyperParams
    -> m (HyperParams)
findHyperParams epochs net optInfo@HyperParamOptimisationInfo {..} fu0 params0 = do
    start <- liftIO getCurrentTime
    (params, fu, valAcc) <- updateHyperParams epochs updateFactor net trainSet valSet fu0 params0
    finish <- liftIO getCurrentTime
    liftIO . print $ diffUTCTime finish start
    if valAcc > requiredAcc
        then pure params
        else let newOptInfo = optInfo { updateFactor = updateFactor * updateFactorDecay }
             in findHyperParams epochs net newOptInfo fu params

data HyperParamOptimisationInfo i o = HyperParamOptimisationInfo
    { updateFactor :: Double
    , updateFactorDecay :: Double
    , trainSet :: DataSet i o
    , valSet :: DataSet i o
    , requiredAcc :: Accuracy
    } deriving (Show)

findHyperParamsWithSeveralRuns ::
       forall (shapes :: [Shape]) (layers :: [*]) (i :: Shape) (o :: Shape) (m :: * -> *).
       ( SingI o
       , i ~ Head shapes
       , o ~ Last shapes
       , MonadIO m
       , SumSquaredParams (Network layers shapes)
       )
    => Int
    -> Network layers shapes
    -> [HyperParamOptimisationInfo i o]
    -> HyperParams
    -> m (HyperParams)
findHyperParamsWithSeveralRuns _ _ [] params = pure params
findHyperParamsWithSeveralRuns epochs net (x@HyperParamOptimisationInfo{..}:xs) params0 = do
    params <- findHyperParams epochs net x Regulator params0
    liftIO . putStrLn . showAccuracy "validation" . getAccuracy $ getHyperParamInfo epochs net trainSet valSet params
    findHyperParamsWithSeveralRuns epochs net xs params
