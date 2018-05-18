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
    , trainNetworkAndPrintAccuracies
    ) where

import Grenade.Core
import Grenade.Train.DataSet
import Grenade.Train.Test
import Grenade.Utils.Accuracy

import Data.List
import Data.List.Extra

import Data.Singletons (SingI)
import Data.Singletons.Prelude (Head, Last)

import Control.Monad.IO.Class

-- Train the network while printing the training, validation and test accuracy after every full run.
trainNetworkAndPrintAccuracies ::
       forall (shapes :: [Shape]) (layers :: [*]) (i :: Shape) (o :: Shape) (m :: * -> *).
       (SingI o, i ~ Head shapes, o ~ Last shapes, MonadIO m)
    => Int
    -> Int
    -> LearningParameters
    -> DataSet i o
    -> DataSet i o
    -> DataSet i o
    -> Network layers shapes
    -> m (Network layers shapes)
trainNetworkAndPrintAccuracies _ 0 _ _ _ testSet net = do
    liftIO . putStrLn . showClassificationAccuracy "test" $ accuracy net testSet
    pure $ net
trainNetworkAndPrintAccuracies batchsize n params trainSet valSet testSet net0 = do
    let net = runIteration batchsize params trainSet net0
    liftIO . putStrLn . showClassificationAccuracy "train" $
        accuracy net trainSet
    trainNetworkAndPrintAccuracies
        batchsize
        (n - 1)
        params
        trainSet
        valSet
        testSet
        net

-- Train the network
trainNetwork ::
       forall (shapes :: [Shape]) (layers :: [*]) (i :: Shape) (o :: Shape).
       (SingI o, i ~ Head shapes, o ~ Last shapes)
    => Int
    -> Int
    -> LearningParameters
    -> DataSet i o
    -> Network layers shapes
    -> Network layers shapes
trainNetwork _ 0 _ _ net = net
trainNetwork batchsize n params trainSet net0 =
    trainNetwork batchsize (n - 1) params trainSet $
    runIteration batchsize params trainSet net0

-- Train the network by one full run
runIteration ::
       forall (shapes :: [Shape]) (layers :: [*]) (i :: Shape) (o :: Shape).
       (SingI o, i ~ Head shapes, o ~ Last shapes)
    => Int
    -> LearningParameters
    -> DataSet i o
    -> Network layers shapes
    -> Network layers shapes
runIteration batchsize params trainSet net =
    let chunks = chunksOf batchsize trainSet
     in foldl' runBatch net chunks
  where
    runBatch network chunk =
        let grads =
                fmap
                    (\(inptPoint, outptPoint) ->
                         backPropagate network inptPoint outptPoint)
                    chunk
         in foldl'
                (\tempNet grad -> applyUpdate params tempNet grad)
                network
                grads
