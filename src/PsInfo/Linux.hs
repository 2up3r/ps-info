{-# LANGUAGE DataKinds, FlexibleContexts, GADTs #-}
module PsInfo.Linux where

import PsInfo.Linux.Proc ( getPIDCPUTime, getCPUTime, getMemUsage, getMemTotal, getPIDMEMUsage )

import Control.Concurrent ( threadDelay )
import Control.Monad.Freer ( send, Eff, Members )
import Control.Monad.Freer.Error ( throwError, Error )

type PID = Integer
type MicroSec = Int

getPIDCPUPercent :: (Members '[Error String, IO] r) => PID -> MicroSec -> Eff r Double
getPIDCPUPercent pid delay = do
    pidTime0 <- getPIDCPUTime pid
    sysTime0 <- getCPUTime
    send $ threadDelay delay
    pidTime1 <- getPIDCPUTime pid
    sysTime1 <- getCPUTime
    let pidDiff = pidTime1 - pidTime0
        sysDiff = sysTime1 - sysTime0
    if sysDiff == 0
        then throwError "No change in system time"
        else pure $ fromIntegral pidDiff / fromIntegral sysDiff

getPIDMemPercent :: (Members '[Error String, IO] r) => PID -> Eff r Double
getPIDMemPercent pid = do
    memTotal <- getMemTotal
    memPID <- getPIDMEMUsage pid
    if memTotal == 0
        then throwError "Error: Memory avalible is 0."
        else pure $ fromIntegral memPID / fromIntegral memTotal

getMemPercent :: (Members '[Error String, IO] r) => Eff r Double
getMemPercent = do
    memTotal <- getMemTotal
    memPID <- getMemUsage
    if memTotal == 0
        then throwError "Error: Memory avalible is 0."
        else pure $ fromIntegral memPID / fromIntegral memTotal
