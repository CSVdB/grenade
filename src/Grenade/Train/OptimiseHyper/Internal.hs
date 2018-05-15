{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Grenade.Train.OptimiseHyper.Internal where

import Grenade.Core
import Grenade.Train.DataSet
import Grenade.Train.HyperParamInfo
import Grenade.Train.HyperParamInfo.Internal
import Grenade.Train.HyperParams
import Grenade.Utils.Accuracy
import Grenade.Utils.LogDouble
import Grenade.Utils.PositiveDouble
import Grenade.Utils.PositiveDouble.Internal
import Grenade.Utils.PositiveInt
import Grenade.Utils.ProperFraction

import GHC.Generics

import Data.List.Extra (maximumOn)
import Data.Validity

import Control.Monad
import Control.Monad.Random.Class

import Data.Singletons (SingI)
import Data.Singletons.Prelude (Head, Last)

import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Time.Clock

import Numeric.Natural

nOfValues :: Int
nOfValues = 20

data FieldToUpdate
    = Rate
    | Momentum
    | Decay
    deriving (Show, Eq, Generic)

instance Validity FieldToUpdate

nextFu :: FieldToUpdate -> FieldToUpdate
nextFu Rate = Momentum
nextFu Momentum = Decay
nextFu Decay = Rate

changeParams :: FieldToUpdate -> HyperParams -> PositiveDouble -> HyperParams
changeParams Rate (HyperParams lparams@LearningParameters {..} pf) x =
    HyperParams lparams {learningRate = pMultiply x learningRate} pf
changeParams Momentum (HyperParams lparams@LearningParameters {..} pf) x =
    HyperParams
        lparams {learningMomentum = properFractionMultiply x learningMomentum}
        pf
changeParams Decay params@(HyperParams lparams pf) x =
    case HyperParams lparams <$> dMultiply x pf of
        Nothing -> params
        Just newParams -> newParams

genParams ::
       MonadRandom m
    => FieldToUpdate
    -> LogDouble
    -> HyperParams
    -> m HyperParams
genParams fu uf params = do
    let ufDouble = logToDouble uf
    newParams <-
        changeParams fu params . PositiveDouble . exp <$>
        getRandomR (-ufDouble, ufDouble)
    if isValid newParams
        then pure newParams
        else genParams fu uf params

updateRegulariser :: HyperParamInfo -> LogDouble -> HyperParams
updateRegulariser info@(HyperParamInfo (HyperParams lp@LearningParameters {..} decayRate) _) alpha =
    let ratio = pExp (quotientOfParamsSum info) alpha
     in HyperParams
            lp {learningRegulariser = pMultiply learningRegulariser ratio}
            decayRate

updateHyperParams ::
       forall (shapes :: [Shape]) (layers :: [*]) (i :: Shape) (o :: Shape) (m :: * -> *).
       ( SingI o
       , i ~ Head shapes
       , o ~ Last shapes
       , MonadRandom m
       , MetricNormedSpace (Network layers shapes)
       )
    => Int
    -> LogDouble
    -> Network layers shapes
    -> DataSet i o
    -> DataSet i o
    -> FieldToUpdate
    -> HyperParams
    -> LogDouble
    -> m (HyperParams, FieldToUpdate, Accuracy)
updateHyperParams epochs updateFactor net trainSet valSet fu params alpha = do
    paramSets <- genParamSets fu updateFactor params
    let bestParamInfo =
            maximumOn getAccuracy $
            getHyperParamInfo epochs net trainSet valSet <$> paramSets
        valAcc = getAccuracy bestParamInfo
        newParams = updateRegulariser bestParamInfo alpha
    pure (newParams, nextFu fu, valAcc)
  where
    genParamSets a b c = replicateM nOfValues $ genParams a b c

findHyperParams ::
       forall (shapes :: [Shape]) (layers :: [*]) (i :: Shape) (o :: Shape) (m :: * -> *).
       ( SingI o
       , i ~ Head shapes
       , o ~ Last shapes
       , MonadIO m
       , MonadRandom m
       , MetricNormedSpace (Network layers shapes)
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
    (params, fu, valAcc) <-
        liftIO $
        updateHyperParams
            epochs
            updateFactorTrain
            net
            trainSet
            valSet
            fu0
            params0
            alpha
    finish <- liftIO getCurrentTime
    liftIO . print $ diffUTCTime finish start
    liftIO . putStrLn $ showAccuracy "validation" valAcc
    liftIO . putStrLn $ prettyPrintHyperParams params
    if valAcc > requiredAccTrain || maxIterTrain == 0
        then pure params
        else let newTrainInfo =
                     trainInfo
                         { updateFactorTrain =
                               decayLogDouble
                                   updateFactorTrain
                                   updateProperFractionTrain
                         , maxIterTrain = pred maxIterTrain
                         }
              in findHyperParams epochs net newTrainInfo fu params

data TrainInfo i o = TrainInfo
    { updateFactorTrain :: !LogDouble
    , updateProperFractionTrain :: !ProperFraction
    , trainSet :: !(DataSet i o)
    , valSet :: !(DataSet i o)
    , requiredAccTrain :: !Accuracy
    , maxIterTrain :: !Natural
    , alpha :: !LogDouble
    } deriving (Show, Generic)

instance Validity (TrainInfo (i :: Shape) (o :: Shape))

getTrainInfo ::
       forall (i :: Shape) (o :: Shape).
       HyperParamOptInfo
    -> DataSet i o
    -> DataSet i o
    -> TrainInfo i o
getTrainInfo HyperParamOptInfo {..} fullTrainSet fullValSet =
    TrainInfo
        updateFactor
        updateProperFraction
        (takePos trainSize fullTrainSet)
        (takePos valSize fullValSet)
        requiredAcc
        maxIterations
        alphaInfo

data HyperParamOptInfo = HyperParamOptInfo
    { updateFactor :: !LogDouble
    , updateProperFraction :: !ProperFraction
    , trainSize :: !PositiveInt
    , valSize :: !PositiveInt
    , requiredAcc :: !Accuracy
    , maxIterations :: !Natural
    , alphaInfo :: !LogDouble
    } deriving (Show)

instance Validity HyperParamOptInfo where
    validate HyperParamOptInfo {..} =
        mconcat
            [ updateFactor <?!> "Update factor"
            , updateProperFraction <?!> "Update factor decay"
            , trainSize <?!> "Training data size"
            , valSize <?!> "Validation data size"
            , requiredAcc <?!> "Required accuracy"
            , maxIterations <?!> "Maximum number of iterations"
            , alphaInfo <?!> "Alpha"
            ]

showIfError :: Either SomeException a -> Either String a
showIfError x =
    case x of
        Left err -> Left $ displayException err
        Right y -> Right y

getHyperParamOptInfo ::
       LogDouble
    -> ProperFraction
    -> PositiveInt
    -> PositiveInt
    -> Double
    -> Natural
    -> LogDouble
    -> Either String HyperParamOptInfo
getHyperParamOptInfo uf udf ts vs ra mi al = do
    acc <- showIfError $ accuracyM ra
    prettyValidation $ HyperParamOptInfo uf udf ts vs acc mi al

findHyperParamsWithSeveralRuns ::
       forall (shapes :: [Shape]) (layers :: [*]) (i :: Shape) (o :: Shape) (m :: * -> *).
       ( SingI o
       , i ~ Head shapes
       , o ~ Last shapes
       , MonadIO m
       , MonadRandom m
       , MetricNormedSpace (Network layers shapes)
       )
    => Int
    -> Network layers shapes
    -> DataSet i o
    -> DataSet i o
    -> [HyperParamOptInfo]
    -> HyperParams
    -> m (HyperParams)
findHyperParamsWithSeveralRuns _ _ _ _ [] params = pure params
findHyperParamsWithSeveralRuns epochs net trainSet valSet (x@HyperParamOptInfo {..}:xs) params0 = do
    params <-
        findHyperParams epochs net (getTrainInfo x trainSet valSet) Rate params0
    liftIO . putStrLn . showAccuracy "validation" . getAccuracy $
        getHyperParamInfo epochs net trainSet valSet params
    findHyperParamsWithSeveralRuns epochs net trainSet valSet xs params
