module Grenade.Utils.PositiveDouble
    ( PositiveDouble
    , positiveToDouble
    , pMultiply
    , pExp
    , constructPositiveDouble
    ) where

import Grenade.Utils.PositiveDouble.Internal

positiveToDouble :: PositiveDouble -> Double
positiveToDouble (PositiveDouble x) = x

pMultiply :: PositiveDouble -> PositiveDouble -> PositiveDouble
pMultiply (PositiveDouble x) (PositiveDouble y) = PositiveDouble $ x * y

pExp :: PositiveDouble -> Double -> PositiveDouble
pExp (PositiveDouble x) y = PositiveDouble $ x ** y
