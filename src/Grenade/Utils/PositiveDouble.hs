module Grenade.Utils.PositiveDouble
    ( PositiveDouble
    , positiveToDouble
    , pMultiply
    , constructPositiveDouble
    ) where

import Grenade.Utils.PositiveDouble.Internal

positiveToDouble :: PositiveDouble -> Double
positiveToDouble (PositiveDouble x) = x

pMultiply :: PositiveDouble -> PositiveDouble -> PositiveDouble
pMultiply (PositiveDouble x) (PositiveDouble y) = PositiveDouble $ x * y
