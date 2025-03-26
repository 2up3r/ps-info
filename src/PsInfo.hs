{-# LANGUAGE OverloadedStrings, DataKinds, FlexibleContexts, GADTs #-}
module PsInfo where

-- import qualified PsInfo.Darwin as D
import qualified PsInfo.Linux as L

import Control.Monad.Freer ( Eff, Members )
import Control.Monad.Freer.Error ( throwError, Error )
import System.Info (os)

type Percent = Double
type PID = Int
type MicroSec = Int

getPIDs :: (Members '[Error String, IO] r) => Eff r [PID]
getPIDs = case os of
    "linux" -> L.getPIDs
    _ -> throwError $ "Not implemented for " ++ os

getProcessCPUUsage :: (Members '[Error String, IO] r) => PID -> MicroSec -> Eff r Percent
getProcessCPUUsage pid s = case os of
    "linux" -> L.getProcessCPUUsage pid s
    _ -> throwError $ "Not implemented for " ++ os

getProcessMemUsage :: (Members '[Error String, IO] r) => PID -> Eff r Percent
getProcessMemUsage pid = case os of
    "linux" -> L.getProcessMemUsage pid
    _ -> throwError $ "Not implemented for " ++ os

getMemUsage :: (Members '[Error String, IO] r) => Eff r Percent
getMemUsage = case os of
    "linux" -> L.getMemUsage
    _ -> throwError $ "Not implemented for " ++ os

getCPUUsage :: (Members '[Error String, IO] r) => MicroSec -> Eff r Percent
getCPUUsage delay = case os of
    "linux" -> L.getCPUUsage delay
    _ -> throwError $ "Not implemented for " ++ os
