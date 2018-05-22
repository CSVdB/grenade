{-# LANGUAGE DeriveGeneric #-}

module Grenade.Utils.LogDouble
    ( LogDouble
    , pExp
    , logToDouble
    , decayLogDouble
    , constructLogDouble
    , maxLogDouble
    ) where

import Grenade.Utils.PositiveDouble.Internal
import Grenade.Utils.ProperFraction

import Data.Aeson (FromJSON, ToJSON)
import Data.Validity

import GHC.Generics

newtype LogDouble =
    LogDouble Double
    deriving (Show, Eq, Generic)

instance ToJSON LogDouble

instance FromJSON LogDouble

maxLogDouble :: Double
maxLogDouble = 10

logToDouble :: LogDouble -> Double
logToDouble (LogDouble x) = x

instance Validity LogDouble where
    validate (LogDouble x) =
        mconcat
            [ delve "A LogDouble contains a valid double" x
            , declare "A LogDouble is smaller than maxLogDouble" $
              x < maxLogDouble
            , declare "A LogDouble is strictly positive" $ x > 0
            ]

decayLogDouble :: LogDouble -> ProperFraction -> LogDouble
decayLogDouble (LogDouble x) y =
    case prettyValidation . LogDouble $ (*) x $ properToDouble y of
        Left err -> error err
        Right ld -> ld

constructLogDouble :: Double -> Either String LogDouble
constructLogDouble = prettyValidation . LogDouble

pExp :: PositiveDouble -> LogDouble -> PositiveDouble
pExp x (LogDouble y) = PositiveDouble $ (positiveToDouble x) ** y
