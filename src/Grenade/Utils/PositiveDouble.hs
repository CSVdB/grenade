module Grenade.Utils.PositiveDouble
    ( PositiveDouble
    , positiveToDouble
    ) where

import Grenade.Utils.PositiveDouble.Internal

positiveToDouble :: PositiveDouble -> Double
positiveToDouble (PositiveDouble x) = x
