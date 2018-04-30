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
    , HyperParamOptInfo
    , getHyperParamOptInfo
    ) where

import Grenade.Core
import Grenade.Train.DataSet
import Grenade.Train.HyperParamInfo
import Grenade.Train.HyperParamInfo.Internal
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
import Control.Monad.Catch

import Data.Time.Clock
import Data.Validity

import Numeric.Natural

nOfValues :: Int
nOfValues = 20

data FieldToUpdate
    = Rate
    | Momentum
    | Decay
    deriving (Show, Eq, Generic)

nextFu :: FieldToUpdate -> FieldToUpdate
nextFu Rate = Momentum
nextFu Momentum = Decay
nextFu Decay = Rate

changeParams :: FieldToUpdate -> HyperParams -> PositiveDouble -> HyperParams
changeParams Rate (HyperParams lparams@LearningParameters {..} decayFactor) x =
    HyperParams lparams {learningRate = pMultiply x learningRate} decayFactor
changeParams Momentum (HyperParams lparams@LearningParameters {..} decayFactor) x =
    HyperParams lparams {learningMomentum = pMultiply x learningMomentum } decayFactor
changeParams Decay params@(HyperParams lparams decayFactor) x =
    case HyperParams lparams <$> dMultiply x decayFactor of
        Nothing -> params
        Just newParams -> newParams

genParamSets :: MonadIO m => FieldToUpdate -> PositiveDouble -> HyperParams -> m [HyperParams]
genParamSets fu (PositiveDouble uf) params =
    liftIO . gens nOfValues $ changeParams fu params . PositiveDouble . exp <$>
        choose (-uf, uf)

updateHyperParams ::
       forall (shapes :: [Shape]) (layers :: [*]) (i :: Shape) (o :: Shape) (m :: * -> *).
       ( SingI o
       , i ~ Head shapes
       , o ~ Last shapes
       , MonadIO m
       , SumSquaredParams (Network layers shapes)
       )
    => Int
    -> PositiveDouble
    -> Network layers shapes
    -> DataSet i o
    -> DataSet i o
    -> FieldToUpdate
    -> HyperParams
    -> Double
    -> m (HyperParams, FieldToUpdate, Accuracy)
updateHyperParams epochs updateFactor net trainSet valSet fu params alpha = do
    paramSets <- genParamSets fu updateFactor params
    let bestParamInfo = maximumOn getAccuracy $ getHyperParamInfo epochs net trainSet valSet <$> paramSets
        valAcc = getAccuracy bestParamInfo
        newParams = updateRegulariser bestParamInfo alpha
    liftIO . putStrLn $ showAccuracy "validation" valAcc
    pure (newParams, nextFu fu, valAcc)

updateRegulariser :: HyperParamInfo -> Double -> HyperParams
updateRegulariser info@(HyperParamInfo ( HyperParams lp@LearningParameters {..} decayRate) _) alpha =
    let ratio = pExp (quotientOfSumOfWeights info) alpha
    in HyperParams lp { learningRegulariser = pMultiply learningRegulariser ratio } decayRate

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
    -> TrainInfo i o
    -> FieldToUpdate
    -> HyperParams
    -> m (HyperParams)
findHyperParams epochs net trainInfo@TrainInfo {..} fu0 params0 = do
    start <- liftIO getCurrentTime
    liftIO $ print fu0
    (params, fu, valAcc) <- updateHyperParams epochs updateFactorTrain net trainSet valSet fu0 params0 alpha
    finish <- liftIO getCurrentTime
    liftIO . print $ diffUTCTime finish start
    liftIO . putStrLn $ prettyPrintHyperParams params
    if valAcc > requiredAccTrain || maxIterTrain == 0
        then pure params
        else let newTrainInfo = trainInfo { updateFactorTrain = pMultiply updateFactorTrain updateFactorDecayTrain, maxIterTrain = pred maxIterTrain }
             in findHyperParams epochs net newTrainInfo fu params

data TrainInfo i o = TrainInfo
    { updateFactorTrain :: PositiveDouble
    , updateFactorDecayTrain :: PositiveDouble
    , trainSet :: DataSet i o
    , valSet :: DataSet i o
    , requiredAccTrain :: Accuracy
    , maxIterTrain :: Natural
    , alpha :: Double
    } deriving Show

getTrainInfo ::
    forall (i :: Shape) (o :: Shape).
    HyperParamOptInfo -> DataSet i o -> DataSet i o -> TrainInfo i o
getTrainInfo HyperParamOptInfo {..} fullTrainSet fullValSet =
    TrainInfo updateFactor updateFactorDecay (take trainSize fullTrainSet) (take valSize fullValSet) requiredAcc maxIterations alphaInfo

data HyperParamOptInfo = HyperParamOptInfo
    { updateFactor :: PositiveDouble
    , updateFactorDecay :: PositiveDouble
    , trainSize :: Int
    , valSize :: Int
    , requiredAcc :: Accuracy
    , maxIterations :: Natural
    , alphaInfo :: Double
    } deriving Show

instance Validity HyperParamOptInfo where
    validate HyperParamOptInfo {..} = mconcat
        [ updateFactor <?!> "Update factor"
        , updateFactorDecay <?!> "Update factor decay"
        , trainSize > 0 <?@> "The size of the train set is positive"
        , valSize > 0 <?@> "The size of the validation set is positive"
        , requiredAcc <?!> "Required accuracy"
        , maxIterations <?!> "Maximum number of iterations"
        , alphaInfo <?!> "Alpha is a double"
        ]

getHyperParamOptInfo :: Double -> Double -> Int -> Int -> Double -> Natural -> Double -> Either String HyperParamOptInfo
getHyperParamOptInfo uf ufd ts vs ra mi al = do
    acc <- case accuracyM ra of
        Left err -> Left $ displayException err
        Right x -> Right x
    prettyValidation $ HyperParamOptInfo (PositiveDouble uf) (PositiveDouble ufd) ts vs acc mi al

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
    -> DataSet i o
    -> DataSet i o
    -> [HyperParamOptInfo]
    -> HyperParams
    -> m (HyperParams)
findHyperParamsWithSeveralRuns _ _ _ _ [] params = pure params
findHyperParamsWithSeveralRuns epochs net trainSet valSet (x@HyperParamOptInfo{..}:xs) params0 = do
    params <- findHyperParams epochs net (getTrainInfo x trainSet valSet) Rate params0
    liftIO . putStrLn . showAccuracy "validation" . getAccuracy $ getHyperParamInfo epochs net trainSet valSet params
    findHyperParamsWithSeveralRuns epochs net trainSet valSet xs params
