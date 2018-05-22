--import Control.Monad
--import qualified Test.Grenade.Network
--
--import qualified Test.Grenade.Layers.Convolution
--import qualified Test.Grenade.Layers.FullyConnected
--import qualified Test.Grenade.Layers.Nonlinear
--import qualified Test.Grenade.Layers.PadCrop
--import qualified Test.Grenade.Layers.Pooling
--
--import qualified Test.Grenade.Layers.Internal.Convolution
--import qualified Test.Grenade.Layers.Internal.Pooling
--
import qualified Test.Grenade.QuickCheck.Layers.Spec

--import qualified Test.Grenade.QuickCheck.Train.HyperParamInfoSpec
--import qualified Test.Grenade.QuickCheck.Train.NetworkSpec
--import qualified Test.Grenade.QuickCheck.Train.OptimiseHyper.InstanceSpec
--import qualified Test.Grenade.QuickCheck.Train.OptimiseHyper.Spec
--import qualified Test.Grenade.QuickCheck.Utils.InstanceSpec
--import qualified Test.Grenade.QuickCheck.Utils.Spec
--import qualified Test.Grenade.Recurrent.Layers.LSTM
--import System.Exit
--import System.IO
import Test.Hspec.Core.Runner

config :: Config
config =
    defaultConfig
        { configPrintCpuTime = False
        -- , configColorMode = ColorAlways
        }

main :: IO ()
main = hspecWith config $ Test.Grenade.QuickCheck.Layers.Spec.spec
          --Test.Grenade.Network.tests
        --, Test.Grenade.QuickCheck.Train.OptimiseHyper.InstanceSpec.tests
        --, Test.Grenade.QuickCheck.Train.OptimiseHyper.Spec.tests
        --,
        --, Test.Grenade.Layers.Pooling.tests
        --, Test.Grenade.Layers.Convolution.tests
        --, Test.Grenade.Layers.FullyConnected.tests
        --, Test.Grenade.Layers.Nonlinear.tests
        --, Test.Grenade.Layers.PadCrop.tests
        --, Test.Grenade.Layers.Internal.Convolution.tests
        --, Test.Grenade.Layers.Internal.Pooling.tests
        --, Test.Grenade.Recurrent.Layers.LSTM.tests
        --, Test.Grenade.QuickCheck.Utils.InstanceSpec.tests
        --, Test.Grenade.QuickCheck.Utils.Spec.tests
        --, Test.Grenade.QuickCheck.Train.NetworkSpec.tests
        --, Test.Grenade.QuickCheck.Train.HyperParamInfoSpec.tests
--        disorderMain
--        [ Test.Grenade.QuickCheck.Layers.Spec.tests
--        ]
--
-- disorderMain :: [IO Bool] -> IO ()
-- disorderMain tests = do
--     lineBuffer
--     rs <- sequence tests
--     unless (and rs) exitFailure
--
-- lineBuffer :: IO ()
-- lineBuffer = do
--     hSetBuffering stdout LineBuffering
--     hSetBuffering stderr LineBuffering
