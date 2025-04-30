{-# LANGUAGE DataKinds #-}
module Main where

import Control.Monad (forM)
import Foreign.C (CInt)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import Control.Monad.Freer (Eff, runM, send)
import Control.Monad.Freer.Error (Error, runError)

import PsInfo.Darwin.Libproc (getProcTaskInfo, ProcTaskInfo)
import Text.Read (readMaybe)

main :: IO ()
main = do
    args <- getArgs
    er <- runM $ runError $ handler args
    case er of
        (Left _) -> exitFailure
        (Right _) -> exitSuccess

handler :: [String] -> Eff '[Error String, IO] ()
handler pids = do
    ptis <- send $ forM pids getPTI
    send $ print ptis
    where
        getPTI :: String -> IO (Maybe ProcTaskInfo)
        getPTI pid = do
            let mpid = readMaybe pid :: Maybe CInt
            case mpid of
                Nothing -> pure Nothing
                (Just pid') -> do
                    epti <- runM $ runError $ getProcTaskInfo pid' :: IO (Either String ProcTaskInfo)
                    case epti of
                        (Left _) -> pure Nothing
                        (Right pti) -> pure $ Just pti
