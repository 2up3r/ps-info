{-# LANGUAGE DataKinds, FlexibleContexts, GADTs #-}
module PsInfo.Linux where

import qualified PsInfo.Linux.Proc as P

import Control.Concurrent ( threadDelay )
import Control.Monad.Freer ( send, Eff, Members )
import Control.Monad.Freer.Error ( throwError, Error )

type PID = Int
type MicroSec = Int

getPIDs :: (Members '[Error String, IO] r) => Eff r [PID]
getPIDs = send P.getPIDs

getProcessCPUUsage :: (Members '[Error String, IO] r) => PID -> MicroSec -> Eff r Double
getProcessCPUUsage pid delay = do
    pidTime0 <- P.getProcessCPUTime pid
    sysTime0 <- P.getCPUTime
    send $ threadDelay delay
    pidTime1 <- P.getProcessCPUTime pid
    sysTime1 <- P.getCPUTime
    let pidDiff = pidTime1 - pidTime0
        sysDiff = sysTime1 - sysTime0
    if sysDiff == 0
        then throwError "No change in CPU time."
        else pure $ fromIntegral pidDiff / fromIntegral sysDiff

getProcessMemUsage :: (Members '[Error String, IO] r) => PID -> Eff r Double
getProcessMemUsage pid = do
    memTotal <- P.getMemTotal
    memPID <- P.getProcessMemUsage pid
    if memTotal == 0
        then throwError "Memory avalible is 0."
        else pure $ fromIntegral memPID / fromIntegral memTotal

getMemUsage :: (Members '[Error String, IO] r) => Eff r Double
getMemUsage = do
    memTotal <- P.getMemTotal
    memPID <- P.getMemActive
    if memTotal == 0
        then throwError "Memory avalible is 0."
        else pure $ fromIntegral memPID / fromIntegral memTotal

getCPUUsage :: (Members '[Error String, IO] r) => MicroSec -> Eff r Double
getCPUUsage delay = do
    st0 <- P.getCPUTime
    ac0 <- P.getCPUActive
    send $ threadDelay delay
    st1 <- P.getCPUTime
    ac1 <- P.getCPUActive
    let st = st1 - st0
        ac = ac1 - ac0
    if st == 0
        then throwError "No change in CPU time."
        else pure $ fromIntegral ac / fromIntegral st
