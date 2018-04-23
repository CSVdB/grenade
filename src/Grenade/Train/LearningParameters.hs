{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Grenade.Train.LearningParameters
    ( HyperParamInfo
    , Accuracy
    , accuracyM
    , getHyperParamInfo
    ) where

import Grenade.Core
import Grenade.Train.LearningParameters.Internal
import Grenade.Train.DataSet
import Grenade.Train.Network
import Grenade.Utils.Accuracy
import Grenade.Utils.SumSquaredParams

import Data.Singletons (SingI)
import Data.Singletons.Prelude (Last, Head)

getHyperParamInfo
    :: forall (shapes :: [Shape]) (layers :: [*]) (i :: Shape) (o :: Shape)
    . (SingI o, i ~ Head shapes, o ~ Last shapes,
        SumSquaredParams (Network layers shapes))
    => Int
    -> LearningParameters
    -> Network layers shapes
    -> DataSet i o
    -> DataSet i o
    -> HyperParamInfo
getHyperParamInfo 0 params _ _ _ = initHyperParamInfo params
getHyperParamInfo n params net0 trainSet valSet =
    let (net, iterRunInfo) = getNetAndRunInfo params trainSet valSet net0
    in updateHyperParamInfo iterRunInfo $ getHyperParamInfo (n - 1) params net trainSet valSet
