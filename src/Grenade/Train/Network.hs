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
    , trainNetworkAndPrintAccuracies
    , getNetAndRunInfo
    ) where

import Grenade.Core
import Grenade.Train.DataSet
import Grenade.Train.HyperParamInfo.Internal
import Grenade.Train.HyperParams
import Grenade.Train.Test
import Grenade.Utils.Accuracy

import Data.List

import Data.Singletons (SingI)
import Data.Singletons.Prelude (Head, Last)

import Control.Monad.IO.Class

-- Train the network while printing the training, validation and test accuracy after every full run.
trainNetworkAndPrintAccuracies ::
       forall (shapes :: [Shape]) (layers :: [*]) (i :: Shape) (o :: Shape) (m :: * -> *).
       ( SingI o
       , i ~ Head shapes
       , o ~ Last shapes
       , MonadIO m
       , MetricNormedSpace (Network layers shapes)
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
       , MetricNormedSpace (Network layers shapes)
       )
    => HyperParams
    -> DataSet i o
    -> DataSet i o
    -> Network layers shapes
    -> (Network layers shapes, RunInfo)
getNetAndRunInfo params trainSet valSet net0 =
    let net = runIteration params trainSet net0
        trainAcc = accuracy net trainSet
        valAcc = accuracy net valSet
        netNorm = norm net
        netDistance = distance net0 net
        iterRunInfo = RunInfo trainAcc valAcc netNorm netDistance
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
