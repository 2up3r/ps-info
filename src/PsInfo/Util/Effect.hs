{-# LANGUAGE DataKinds #-}
module PsInfo.Util.Effect (eitherToEff) where

import Control.Monad.Freer (Eff, Members, send)
import Control.Monad.Freer.Error (Error, throwError)

eitherToEff :: Members '[Error e, IO] r => IO (Either e a) -> Eff r a
eitherToEff io = do
    ev <- send io
    case ev of
        (Left e) -> throwError e
        (Right v) -> pure v
