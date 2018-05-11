module Grenade.Utils.PositiveInt
    ( PositiveInt
    , positiveToInt
    , constructPositiveInt
    , takePos
    ) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import Grenade.Utils.PositiveInt.Internal

positiveToInt :: PositiveInt -> Int
positiveToInt (PositiveInt n) = n

takePos :: PositiveInt -> NonEmpty a -> NonEmpty a
takePos (PositiveInt n) = NEL.fromList . NEL.take n
