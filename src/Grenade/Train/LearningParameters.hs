{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Grenade.Train.LearningParameters
    ( HyperParamInfo
    , Accuracy
    , accuracyM
    ) where

import Grenade.Train.LearningParameters.Internal
import Grenade.Utils.Accuracy
