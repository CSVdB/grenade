module Grenade.Utils.PositiveDouble
    ( PositiveDouble
    , positiveToDouble
    , pMultiply
    , constructPositiveDouble
    , eitherPositiveDouble
    , constructPosDoubleUnsafe
    , pDivide
    ) where

import Grenade.Utils.PositiveDouble.Internal

import Control.Monad.Catch

pMultiply :: PositiveDouble -> PositiveDouble -> PositiveDouble
pMultiply (PositiveDouble x) (PositiveDouble y) =
    case constructPositiveDouble $ x * y of
        Right z -> z
        Left err -> error $ displayException err

pDivide :: PositiveDouble -> PositiveDouble -> PositiveDouble
pDivide (PositiveDouble x) (PositiveDouble y) =
    case constructPositiveDouble $ x / y of
        Right z -> z
        Left err -> error $ displayException err
