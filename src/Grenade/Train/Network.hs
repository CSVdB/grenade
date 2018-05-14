{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Grenade.Train.Network
    ( trainNetwork
    , runIteration
    , runIterationAndGetChanges
    , trainNetworkAndPrintAccuracies
    , getNetAndRunInfo
    ) where

import Grenade.Core
import Grenade.Train.DataSet
import Grenade.Train.HyperParamInfo.Internal
import Grenade.Train.HyperParams
import Grenade.Train.Test
import Grenade.Utils.Accuracy
import Grenade.Utils.SumSquaredParams

import Data.List
import Data.Proxy
import Data.Validity

import Data.Singletons (SingI)
import Data.Singletons.Prelude (Head, Last)

import Control.Monad.IO.Class

import qualified Data.List.NonEmpty as NEL

-- Train the network while printing the training, validation and test accuracy after every full run.
trainNetworkAndPrintAccuracies ::
       forall (shapes :: [Shape]) (layers :: [*]) (i :: Shape) (o :: Shape) (m :: * -> *).
       ( SingI o
       , i ~ Head shapes
       , o ~ Last shapes
       , MonadIO m
       , SumSquaredParams (Network layers shapes)
       )
    => Int
    -> HyperParams
    -> DataSet i o
    -> DataSet i o
    -> DataSet i o
    -> Network layers shapes
    -> m (Network layers shapes)
trainNetworkAndPrintAccuracies 0 _ _ _ testSet net = do
    liftIO . putStrLn . showAccuracy "test" $ accuracy net testSet
    pure net
trainNetworkAndPrintAccuracies n params trainSet valSet testSet net0 = do
    let (net, runInfo) = getNetAndRunInfo params trainSet valSet net0
    liftIO . putStrLn $ prettyPrintRunInfo runInfo
    trainNetworkAndPrintAccuracies
        (n - 1)
        (decay params)
        trainSet
        valSet
        testSet
        net

-- Train the network
trainNetwork ::
       forall (shapes :: [Shape]) (layers :: [*]) (i :: Shape) (o :: Shape).
       (SingI o, i ~ Head shapes, o ~ Last shapes)
    => Int
    -> HyperParams
    -> DataSet i o
    -> Network layers shapes
    -> Network layers shapes
trainNetwork 0 _ _ net = net
trainNetwork n params trainSet net0 =
    trainNetwork (n - 1) (decay params) trainSet $
    runIteration params trainSet net0

getNetAndRunInfo ::
       forall (shapes :: [Shape]) (layers :: [*]) (i :: Shape) (o :: Shape).
       ( SingI o
       , i ~ Head shapes
       , o ~ Last shapes
       , SumSquaredParams (Network layers shapes)
       )
    => HyperParams
    -> DataSet i o
    -> DataSet i o
    -> Network layers shapes
    -> (Network layers shapes, RunInfo)
getNetAndRunInfo params trainSet valSet net0 =
    let (net, sizeOfDeltaWeights0) =
            runIterationAndGetChanges params trainSet net0
        trainAcc = accuracy net trainSet
        valAcc = accuracy net valSet
        sizeOfWeights0 = getSumSquaredParams net
        iterRunInfo = RunInfo trainAcc valAcc sizeOfWeights0 sizeOfDeltaWeights0
     in (net, iterRunInfo)

-- Train the network by one full run
runIteration ::
       forall (shapes :: [Shape]) (layers :: [*]) (i :: Shape) (o :: Shape).
       (SingI o, i ~ Head shapes, o ~ Last shapes)
    => HyperParams
    -> DataSet i o
    -> Network layers shapes
    -> Network layers shapes
runIteration HyperParams {..} trainSet net0 =
    foldl'
        (\net (inpt, outpt) -> train learningParams net inpt outpt)
        net0
        trainSet

-- trainAndGetChanges for a data set
runIterationAndGetChanges ::
       forall (shapes :: [Shape]) (layers :: [*]) (i :: Shape) (o :: Shape).
       ( SingI o
       , i ~ Head shapes
       , o ~ Last shapes
       , SumSquaredParams (Network layers shapes)
       )
    => HyperParams
    -> DataSet i o
    -> Network layers shapes
    -> (Network layers shapes, PositiveDouble)
runIterationAndGetChanges params dataset net0 =
    foldl' update (trainAndGetChanges params (NEL.head dataset) net0) $
    NEL.tail dataset
  where
    update (!network0, !currentSum) datapoint =
        mappend currentSum <$> trainAndGetChanges params datapoint network0

-- Train the network by one full run, while storing sum (delta p)^2
-- where the sum is over all parameters p in the network. This is a useful
-- measure for finding new learningparameters after a few runs.
trainAndGetChanges ::
       forall (shapes :: [Shape]) (layers :: [*]) (i :: Shape) (o :: Shape).
       ( SingI o
       , i ~ Head shapes
       , o ~ Last shapes
       , SumSquaredParams (Network layers shapes)
       )
    => HyperParams
    -> DataPoint i o
    -> Network layers shapes
    -> (Network layers shapes, PositiveDouble)
trainAndGetChanges HyperParams {..} (inpt, outpt) net0 =
    let grad = backPropagate net0 inpt outpt
        proxy = Proxy @(Network layers shapes)
     in ( applyUpdate learningParams net0 grad
        , getSumSquaredParamsDelta proxy grad)
