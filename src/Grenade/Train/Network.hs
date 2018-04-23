{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Grenade.Train.Network
    ( trainNetwork
    , runIteration
    , runIterationAndGetChanges
    , trainNetworkAndPrintAccuracies
    , getNetAndRunInfo
    ) where

import Grenade.Core
import Grenade.Train.DataSet
import Grenade.Train.Test
import Grenade.Train.LearningParameters.Internal
import Grenade.Utils.SumSquaredParams

import Data.List
import Data.Proxy

import Data.Singletons (SingI)
import Data.Singletons.Prelude (Last, Head)

import Control.Monad.IO.Class

-- Train the network while printing the training, validation and test accuracy after every full run.
trainNetworkAndPrintAccuracies
    :: forall (shapes :: [Shape]) (layers :: [*]) (i :: Shape) (o :: Shape) (m :: * -> *)
    . (SingI o, i ~ Head shapes, o ~ Last shapes, MonadIO m,
       SumSquaredParams (Network layers shapes))
    => Int
    -> LearningParameters
    -> DataSet i o
    -> DataSet i o
    -> DataSet i o
    -> Network layers shapes
    -> m (Network layers shapes)
trainNetworkAndPrintAccuracies 0 _ _ _ _ net = pure $ net
trainNetworkAndPrintAccuracies n params trainSet valSet testSet net0 = do
    let (net, runInfo) = getNetAndRunInfo params trainSet valSet testSet net0
    prettyPrintRunInfo runInfo
    trainNetworkAndPrintAccuracies (n - 1) params trainSet valSet testSet net

-- Train the network
trainNetwork
    :: forall (shapes :: [Shape]) (layers :: [*]) (i :: Shape) (o :: Shape)
    . (SingI o, i ~ Head shapes, o ~ Last shapes)
    => Int
    -> LearningParameters
    -> DataSet i o
    -> Network layers shapes
    -> Network layers shapes
trainNetwork 0 _ _ net = net
trainNetwork n params trainSet net0 = trainNetwork (n - 1) params trainSet $ runIteration params trainSet net0

getNetAndRunInfo
    :: forall (shapes :: [Shape]) (layers :: [*]) (i :: Shape) (o :: Shape)
    . (SingI o, i ~ Head shapes, o ~ Last shapes,
        SumSquaredParams (Network layers shapes))
    => LearningParameters
    -> DataSet i o
    -> DataSet i o
    -> DataSet i o
    -> Network layers shapes
    -> (Network layers shapes, RunInfo)
getNetAndRunInfo params trainSet valSet testSet net0 =
    let (net, sizeOfDeltaWeights0) = runIterationAndGetChanges params trainSet net0
        trainAcc = accuracy net trainSet
        valAcc = accuracy net valSet
        testAcc = accuracy net testSet
        sizeOfWeights0 = getSumSquaredParams net
        iterRunInfo = RunInfo trainAcc valAcc testAcc sizeOfWeights0 sizeOfDeltaWeights0
    in (net, iterRunInfo)

-- Train the network by one full run
runIteration
    :: forall (shapes :: [Shape]) (layers :: [*]) (i :: Shape) (o :: Shape)
    . (SingI o, i ~ Head shapes, o ~ Last shapes)
    => LearningParameters
    -> DataSet i o
    -> Network layers shapes
    -> Network layers shapes
runIteration params trainSet net0 =
    foldl' (\net (inpt, outpt) -> train params net inpt outpt) net0 trainSet

-- trainAndGetChanges for a data set
runIterationAndGetChanges
    :: forall (shapes :: [Shape]) (layers :: [*]) (i :: Shape) (o :: Shape)
    . (SingI o, i ~ Head shapes, o ~ Last shapes,
        SumSquaredParams (Network layers shapes))
    => LearningParameters
    -> DataSet i o
    -> Network layers shapes
    -> (Network layers shapes, WeightSize)
runIterationAndGetChanges _ [] _ = error "The training set used in runIteration is empty"
runIterationAndGetChanges params (x : xs) net0 =
    foldl' update (trainAndGetChanges params x net0) xs
  where
    update (network0, currentSum) datapoint =
        mappend currentSum <$> trainAndGetChanges params datapoint network0

-- Train the network by one full run, while storing sum (delta p)^2
-- where the sum is over all parameters p in the network. This is a useful
-- measure for finding new learningparameters after a few runs.
trainAndGetChanges
    :: forall (shapes :: [Shape]) (layers :: [*]) (i :: Shape) (o :: Shape)
    . (SingI o, i ~ Head shapes, o ~ Last shapes,
        SumSquaredParams (Network layers shapes))
    => LearningParameters
    -> DataPoint i o
    -> Network layers shapes
    -> (Network layers shapes, WeightSize)
trainAndGetChanges params (inpt, outpt) net0 =
    let grad = backPropagate net0 inpt outpt
        proxy = Proxy @(Network layers shapes)
    in (applyUpdate params net0 grad, getSumSquaredParamsDelta proxy grad)
