{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Grenade.Train.HyperParamInfo
    ( HyperParamInfo
    , Accuracy
    , accuracyM
    , getHyperParamInfo
    , getAccuracy
    ) where

import Grenade.Core
import Grenade.Train.DataSet
import Grenade.Train.HyperParamInfo.Internal
import Grenade.Train.HyperParams
import Grenade.Train.Network
import Grenade.Utils.Accuracy

import Data.Singletons (SingI)
import Data.Singletons.Prelude (Head, Last)

import Debug.Trace

getHyperParamInfo ::
       forall (shapes :: [Shape]) (layers :: [*]) (i :: Shape) (o :: Shape).
       ( SingI o
       , i ~ Head shapes
       , o ~ Last shapes
       , MetricNormedSpace (Network layers shapes)
       )
    => Int
    -> Network layers shapes
    -> DataSet i o
    -> DataSet i o
    -> HyperParams
    -> HyperParamInfo
getHyperParamInfo 0 _ _ _ params = trace "FINISHED" $ initHyperParamInfo params
getHyperParamInfo n net0 trainSet valSet params
    --trace ("sum p^2 = " ++ show (sizeOfWeights iterRunInfo)) $
    --trace ("sum (delta p)^2 = " ++ show (changeOfWeights iterRunInfo)) $
    --updateHyperParamInfo iterRunInfo $
 =
    traceShow (norm net) $
    getHyperParamInfo (n - 1) net trainSet valSet $ decay params
  where
    !net = runIteration params trainSet net0
    --(!net, !iterRunInfo) = getNetAndRunInfo params trainSet valSet net0

getAccuracy :: HyperParamInfo -> Accuracy
getAccuracy (HyperParamInfo _ (x:_)) = validationAccuracy x
getAccuracy h =
    error $
    "The hyperparaminfo has no validation accuracy since it has no runInfos\n" ++
    show h
