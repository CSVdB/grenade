{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Grenade.Layers.FullyConnected
    ( FullyConnected(..)
    , FullyConnected'(..)
    , randomFullyConnected
    ) where

import Control.Monad.Random hiding (fromList)

import Data.Monoid
import Data.Proxy
import Data.Serialize
import Data.Singletons.TypeLits
import Data.Validity

import GHC.Generics (Generic)

--import GHC.TypeLits
import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra.Static hiding ((<>))

import Grenade.Core

import Grenade.Layers.Internal.Update

import Grenade.Utils.ProperFraction

-- | A basic fully connected (or inner product) neural network layer.
data FullyConnected i o =
    FullyConnected !(FullyConnected' i o) -- Neuron weights
                   !(FullyConnected' i o) -- Neuron momentum
    deriving (Generic)

data FullyConnected' i o =
    FullyConnected' !(R o) -- Bias
                    !(L o i) -- Activations
    deriving (Generic)

instance Show (FullyConnected i o) where
    show FullyConnected {} = "FullyConnected"

instance Show (FullyConnected' i o) where
    show FullyConnected' {} = "FullyConnected'"

instance (KnownNat i, KnownNat o) => UpdateLayer (FullyConnected i o) where
    type Gradient (FullyConnected i o) = (FullyConnected' i o)
    runUpdate LearningParameters {..} (FullyConnected (FullyConnected' oldBias oldActivations) (FullyConnected' oldBiasMomentum oldMomentum)) (FullyConnected' biasGradient activationGradient) =
        let doubleRate = positiveToDouble learningRate
            doubleMomentum = properToDouble learningMomentum
            doubleRegulariser = positiveToDouble learningRegulariser
            (newBias, newBiasMomentum) =
                descendVector
                    doubleRate
                    doubleMomentum
                    doubleRegulariser
                    oldBias
                    biasGradient
                    oldBiasMomentum
            (newActivations, newMomentum) =
                descendMatrix
                    doubleRate
                    doubleMomentum
                    doubleRegulariser
                    oldActivations
                    activationGradient
                    oldMomentum
         in FullyConnected
                (FullyConnected' newBias newActivations)
                (FullyConnected' newBiasMomentum newMomentum)
    createRandom = randomFullyConnected

instance (KnownNat i, KnownNat o) =>
         Layer (FullyConnected i o) ('D1 i) ('D1 o) where
    type Tape (FullyConnected i o) ('D1 i) ('D1 o) = R i
  -- Do a matrix vector multiplication and return the result.
    runForwards (FullyConnected (FullyConnected' wB' wN') _) (S1D v') =
        case prettyValidation (wB', wN', v') of
            Left err ->
                error $
                "This error occurs in the input of runForwards:\n" ++ err
            Right (wB, wN, v) ->
                case prettyValidation (v, S1D (wB + wN #> v)) of
                    Left err ->
                        error $
                        "This error occurs in the output of runForwards:\n" ++
                        err
                    Right output -> output
  -- Run a backpropogation step for a full connected layer.
    runBackwards (FullyConnected (FullyConnected' _ wN') _) x' (S1D dEdy') =
        let (wN, x, dEdy) =
                case prettyValidation (wN', x', dEdy') of
                    Left err ->
                        error $
                        "This error occurs in the input of runBackwards:\n" ++
                        err
                    Right z -> z
            wB' = dEdy
            mm' = dEdy `outer` x
              -- calcluate derivatives for next step
            dWs = tr wN #> dEdy
            output = (FullyConnected' wB' mm', S1D dWs)
         in case prettyValidation output of
                Left err ->
                    error $
                    "This error occurs in the output of runBackwards:\n" ++ err
                Right bla -> bla

instance (KnownNat i, KnownNat o) => Serialize (FullyConnected i o) where
    put (FullyConnected (FullyConnected' b w) _) = do
        putListOf put . LA.toList . extract $ b
        putListOf put . LA.toList . LA.flatten . extract $ w
    get = do
        let f = fromIntegral $ natVal (Proxy :: Proxy i)
        b <-
            maybe (fail "Vector of incorrect size") return .
            create . LA.fromList =<<
            getListOf get
        k <-
            maybe (fail "Vector of incorrect size") return .
            create . LA.reshape f . LA.fromList =<<
            getListOf get
        let bm = konst 0
        let mm = konst 0
        return $ FullyConnected (FullyConnected' b k) (FullyConnected' bm mm)

randomFullyConnected ::
       (MonadRandom m, KnownNat i, KnownNat o) => m (FullyConnected i o)
randomFullyConnected = do
    s1 <- getRandom
    s2 <- getRandom
    let wB = randomVector s1 Uniform * 2 - 1
        wN = uniformSample s2 (-1) 1
        bm = konst 0
        mm = konst 0
    return $ FullyConnected (FullyConnected' wB wN) (FullyConnected' bm mm)

instance (KnownNat i, KnownNat o) =>
         MetricNormedSpace (FullyConnected' i o) where
    zeroM = FullyConnected' zeroM zeroM
    distance (FullyConnected' b w) (FullyConnected' b' w') =
        distance b b' <> distance w w'

instance (KnownNat i, KnownNat o) =>
         MetricNormedSpace (FullyConnected i o) where
    zeroM = FullyConnected zeroM zeroM
    distance (FullyConnected p _) (FullyConnected p' _) = distance p p'

instance (KnownNat i, KnownNat o) => Validity (FullyConnected' i o)

instance (KnownNat i, KnownNat o) => Validity (FullyConnected i o)
