{-# LANGUAGE DataKinds #-}
module PsInfo.Util.Effect 
    ( eitherToEff
    , effToMaybe
    , maybeToEff
    ) where

import Control.Monad.Freer (Eff, Members, send, runM)
import Control.Monad.Freer.Error (Error, throwError, runError)

eitherToEff :: Show e => Members '[Error String, IO] r => IO (Either e a) -> Eff r a
eitherToEff io = do
    ev <- send io
    case ev of
        (Left e) -> throwError $ show e
        (Right v) -> pure v

maybeToEff :: Members '[Error e, IO] r => e -> IO (Maybe a) -> Eff r a
maybeToEff msg io = do
    mv <- send io
    case mv of
        Nothing -> throwError msg
        (Just v) -> pure v

effToMaybe :: Eff '[Error String, IO] a -> IO (Maybe a)
effToMaybe eff = (either (const Nothing) Just <$>) <$> runM $ runError eff
