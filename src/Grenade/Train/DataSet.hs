module Grenade.Train.DataSet where

import Grenade.Core.Shape (S)

import Data.List.NonEmpty

type DataPoint i o = (S i, S o)

type DataSet i o = NonEmpty (DataPoint i o)
