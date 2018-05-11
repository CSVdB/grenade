{-# LANGUAGE DeriveGeneric #-}

module Grenade.Utils.LogDouble
    ( LogDouble
    , pExp
    , logToDouble
    , decayLogDouble
    , constructLogDouble
    , maxLogDouble
    ) where

import Grenade.Utils.ProperFraction
import Grenade.Utils.PositiveDouble.Internal

import Data.Aeson (FromJSON, ToJSON)
import Data.Validity

import GHC.Generics

newtype LogDouble =
    LogDouble Double
    deriving (Show, Eq, Generic)

instance Bounded LogDouble where
    minBound = LogDouble 0
    maxBound = LogDouble maxLogDouble

instance ToJSON LogDouble

instance FromJSON LogDouble

maxLogDouble :: Double
maxLogDouble = 10

logToDouble :: LogDouble -> Double
logToDouble (LogDouble x) = x

instance Validity LogDouble where
    validate (LogDouble x) =
        mconcat
            [ x <?!> "The double is valid"
            , x < maxLogDouble <?!> "The double is not too big"
            , x >= 0 <?!> "The double is positive"
            ]

decayLogDouble :: LogDouble -> ProperFraction -> LogDouble
decayLogDouble (LogDouble x) y = LogDouble $ (*) x $ decayToDouble y

constructLogDouble :: Double -> Either String LogDouble
constructLogDouble = prettyValidation . LogDouble

pExp :: PositiveDouble -> LogDouble -> PositiveDouble
pExp x (LogDouble y) = PositiveDouble $ (positiveToDouble x) ** y
