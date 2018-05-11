module Grenade.Utils.ErrToEither where

import Control.Monad.Catch

errToEither :: Either SomeException a -> Either String a
errToEither (Left err) = Left $ displayException err
errToEither (Right x) = Right x
