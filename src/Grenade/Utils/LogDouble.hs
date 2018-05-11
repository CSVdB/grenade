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
            [ x <?!> "A LogDouble contains a valid double"
            , x < maxLogDouble <?!> "A LogDouble is smaller than maxLogDouble"
            , x > 0 <?!> "A LogDouble is strictly positive"
            ]

decayLogDouble :: LogDouble -> ProperFraction -> LogDouble
decayLogDouble (LogDouble x) y = LogDouble $ (*) x $ properToDouble y

constructLogDouble :: Double -> Either String LogDouble
constructLogDouble = prettyValidation . LogDouble

pExp :: PositiveDouble -> LogDouble -> PositiveDouble
pExp x (LogDouble y) = PositiveDouble $ (positiveToDouble x) ** y
