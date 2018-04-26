module Grenade.Utils.ReadWrite where

import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy as LB

import Data.Maybe
import Data.Serialize (Serialize)
import qualified Data.Serialize as S

import Path
import Path.IO

import Control.Monad.IO.Class

import System.Exit

-- | Read a JSON file, throw an error if the file is missing or there is an
-- error in parsing.
readJSON :: (MonadIO m, FromJSON a) => Path Abs File -> m a
readJSON path = do
    contents <- liftIO $ LB.readFile $ toFilePath path
    case JSON.eitherDecode contents of
        Left decodeErr ->
            liftIO $
            die $
            unwords
                [ "Failed to read JSON file:"
                , toFilePath path
                , "with err"
                , decodeErr
                ]
        Right res -> pure res

-- | Read a JSON file, return a default file if the file is missing.
readJSONWithDefault :: (MonadIO m, FromJSON a) => a -> Path Abs File -> m a
readJSONWithDefault def path = fromMaybe def <$> readJSONWithMaybe path

-- | Read a JSON file, return 'Nothing' if the file is missing.
readJSONWithMaybe :: (MonadIO m, FromJSON a) => Path Abs File -> m (Maybe a)
readJSONWithMaybe path = liftIO $ forgivingAbsence $ readJSON path

-- | Write a JSON file, create the appropriate directories if necessary
writeJSON :: (MonadIO m, ToJSON a) => Path Abs File -> a -> m ()
writeJSON path value =
    liftIO $ do
        ensureDir $ parent path
        LB.writeFile (toFilePath path) $ JSON.encodePretty value

-- readJSON for Serialize
readSerial :: (MonadIO m, Serialize a) => Path Abs File -> m a
readSerial path =
    liftIO $ do
        contents <- liftIO $ LB.readFile $ toFilePath path
        case S.decodeLazy contents of
            Left decodeErr ->
                liftIO $
                die $
                unwords
                    [ "Failed to read Serialize file:"
                    , toFilePath path
                    , "with err"
                    , decodeErr
                    ]
            Right res -> pure res

-- | Read a Serial file, return a default file if the file is missing.
readSerialWithDefault :: (MonadIO m, Serialize a) => a -> Path Abs File -> m a
readSerialWithDefault def path = fromMaybe def <$> readSerialWithMaybe path

-- | Read a Serial file, return 'Nothing' if the file is missing.
readSerialWithMaybe :: (MonadIO m, Serialize a) => Path Abs File -> m (Maybe a)
readSerialWithMaybe path = liftIO $ forgivingAbsence $ readSerial path

-- writeJSON for Serialize
writeSerial :: (MonadIO m, Serialize a) => Path Abs File -> a -> m ()
writeSerial path value =
    liftIO $ do
        ensureDir $ parent path
        LB.writeFile (toFilePath path) $ S.encodeLazy value
