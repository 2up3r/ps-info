{-# LANGUAGE OverloadedStrings, DataKinds, FlexibleContexts, GADTs #-}
module PsInfo 
    ( getCPUActiveTime
    , getCPUTotalTime
    , getCPUUsage
    , getMemUsage
    , getPIDs
    , getProcessCPUUsage
    , getProcessCPUUsages
    , getProcessMemUsage
    , getProcessMemUsages
    , getProcessName
    , getProcessTime
    , getProcessTimes
    , getWallTime
    ) where

import Control.Monad.Freer (Eff, Member, Members)
import Control.Monad.Freer.Error (Error, throwError)
import System.Info (os)

import PsInfo.Util.Types (MicroSecond, Percent, PID)
import qualified PsInfo.Darwin as D
import qualified PsInfo.Linux as L

getCPUActiveTime :: Members '[Error String, IO] r => Eff r MicroSecond
getCPUActiveTime = case os of
    "linux"  -> L.getCPUActiveTime
    "darwin" -> D.getCPUActiveTime
    _        -> throwNotImplemented

getCPUTotalTime :: Members '[Error String, IO] r => Eff r MicroSecond
getCPUTotalTime = case os of
    "linux"  -> L.getCPUTotalTime
    "darwin" -> D.getCPUTotalTime
    _        -> throwNotImplemented

getCPUUsage :: Members '[Error String, IO] r => MicroSecond -> Eff r Percent
getCPUUsage = case os of
    "linux"  -> L.getCPUUsage
    "darwin" -> D.getCPUUsage
    _        -> const throwNotImplemented

getMemUsage :: Members '[Error String, IO] r => Eff r Percent
getMemUsage = case os of
    "linux"  -> L.getMemUsage
    "darwin" -> D.getMemUsage
    _        -> throwNotImplemented

getPIDs :: Members '[Error String, IO] r => Eff r [PID]
getPIDs = case os of
    "linux"  -> L.getPIDs
    "darwin" -> D.getPIDs
    _        -> throwNotImplemented

getProcessCPUUsage :: Members '[Error String, IO] r => PID -> MicroSecond -> Eff r Percent
getProcessCPUUsage = case os of
    "linux"  -> L.getProcessCPUUsage
    "darwin" -> D.getProcessCPUUsage
    _        -> const $ const throwNotImplemented

getProcessCPUUsages :: Members '[Error String, IO] r => [PID] -> MicroSecond -> Eff r [Maybe Percent]
getProcessCPUUsages = case os of
    "linux"  -> L.getProcessCPUUsages
    "darwin" -> D.getProcessCPUUsages
    _        -> const $ const throwNotImplemented

getProcessMemUsage :: Members '[Error String, IO] r => PID -> Eff r Percent
getProcessMemUsage = case os of
    "linux"  -> L.getProcessMemUsage
    "darwin" -> D.getProcessMemUsage
    _        -> const throwNotImplemented

getProcessMemUsages :: Members '[Error String, IO] r => [PID] -> Eff r [Maybe Percent]
getProcessMemUsages = case os of
    "linux"  -> L.getProcessMemUsages
    "darwin" -> D.getProcessMemUsages
    _        -> const throwNotImplemented

getProcessName :: Members '[Error String, IO] r => PID -> Eff r String
getProcessName = case os of
    "linux"  -> L.getProcessName
    "darwin" -> D.getProcessName
    _        -> const throwNotImplemented

getProcessTime :: Members '[Error String, IO] r => PID -> Eff r MicroSecond
getProcessTime = case os of
    "linux"  -> L.getProcessTime
    "darwin" -> D.getProcessTime
    _        -> const throwNotImplemented

getProcessTimes :: Members '[Error String, IO] r => [PID] -> Eff r [Maybe MicroSecond]
getProcessTimes = case os of
    "linux"  -> L.getProcessTimes
    "darwin" -> D.getProcessTimes
    _        -> const throwNotImplemented

getWallTime :: Members '[Error String, IO] r => Eff r MicroSecond
getWallTime = case os of
    "linux"  -> L.getWallTime
    "darwin" -> D.getWallTime
    _        -> throwNotImplemented

throwNotImplemented :: Member (Error String) r => Eff r a
throwNotImplemented = throwError $ "Not implemented for the os: " ++ os
