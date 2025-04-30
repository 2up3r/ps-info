{-# LANGUAGE DataKinds #-}
module PsInfo.Linux 
    ( getCPUActiveTime
    , getCPUTotalTime
    , getCPUUsage
    , getMemUsage
    , P.getPIDs
    , getProcessCPUUsage
    , getProcessMemUsage
    , getProcessName
    , getProcessTime
    , getWallTime
    ) where

import Control.Concurrent (threadDelay)

import Control.Monad.Freer (Eff, Members, send)
import Control.Monad.Freer.Error (Error, throwError)

import PsInfo.Util.Types (MicroSecond, Percent, PID (..), Jiffy)
import qualified PsInfo.Linux.Proc as P

getCPUActiveTime :: Members '[Error String, IO] r => Eff r MicroSecond
getCPUActiveTime = P.getCPUActive >>= jiffyToMicroSecond

getCPUTotalTime :: Members '[Error String, IO] r => Eff r MicroSecond
getCPUTotalTime = P.getCPUTime >>= jiffyToMicroSecond

getCPUUsage :: Members '[Error String, IO] r => MicroSecond -> Eff r Percent
getCPUUsage delay = do
    st0 <- P.getCPUTime
    ac0 <- P.getCPUActive
    send $ threadDelay (fromIntegral delay)
    st1 <- P.getCPUTime
    ac1 <- P.getCPUActive
    let st = st1 - st0
        ac = ac1 - ac0
    if st == 0
        then throwError "No change in CPU time."
        else pure $ fromIntegral ac / fromIntegral st

getMemUsage :: Members '[Error String, IO] r => Eff r Percent
getMemUsage = do
    memTotal <- P.getMemTotal
    memPID <- P.getMemActive
    if memTotal == 0
        then throwError "Memory avalible is 0."
        else pure $ fromIntegral memPID / fromIntegral memTotal

getProcessCPUUsage :: Members '[Error String, IO] r => PID -> MicroSecond -> Eff r Percent
getProcessCPUUsage pid delay = do
    pidTime0 <- P.getProcessCPUTime pid
    sysTime0 <- P.getCPUTime
    send $ threadDelay (fromIntegral delay)
    pidTime1 <- P.getProcessCPUTime pid
    sysTime1 <- P.getCPUTime
    let pidDiff = pidTime1 - pidTime0
        sysDiff = sysTime1 - sysTime0
    if sysDiff == 0
        then throwError "No change in CPU time."
        else pure $ fromIntegral pidDiff / fromIntegral sysDiff

getProcessMemUsage :: Members '[Error String, IO] r => PID -> Eff r Percent
getProcessMemUsage pid = do
    memTotal <- P.getMemTotal
    memPID <- P.getProcessMemUsage pid
    if memTotal == 0
        then throwError "Memory avalible is 0."
        else pure $ fromIntegral memPID / fromIntegral memTotal

getProcessName :: Members '[Error String, IO] r => PID -> Eff r String
getProcessName pid = P.pidStatComm <$> P.getPIDStat pid

getProcessTime :: Members '[Error String, IO] r => PID -> Eff r MicroSecond
getProcessTime pid = P.getProcessCPUTime pid >>= jiffyToMicroSecond

getWallTime :: Members '[Error String, IO] r => Eff r MicroSecond
getWallTime = P.getCPUTime >>= jiffyToMicroSecond

jiffyToMicroSecond :: Members '[Error String, IO] r => Jiffy -> Eff r MicroSecond
jiffyToMicroSecond js = do
    tps <- fromIntegral <$> P.getClockTicksPerSecond
    pure $ (js * 1000000) `div` tps
