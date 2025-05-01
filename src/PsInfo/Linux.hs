{-# LANGUAGE DataKinds #-}
module PsInfo.Linux
    ( getCPUActiveTime
    , getCPUTotalTime
    , getCPUUsage
    , getMemUsage
    , P.getPIDs
    , getProcessCPUUsage
    , getProcessCPUUsages
    , getProcessMemUsage
    , getProcessMemUsages
    , getProcessName
    , getProcessTime
    , getProcessTimes
    , getWallTime
    ) where

import Control.Concurrent (threadDelay)

import Control.Monad.Freer (Eff, Members, send, runM)
import Control.Monad.Freer.Error (Error, throwError, runError)

import PsInfo.Util.Types (Jiffy, MicroSecond, Percent, PID (..))
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

getProcessCPUUsages :: Members '[Error String, IO] r => [PID] -> MicroSecond -> Eff r [Maybe Percent]
getProcessCPUUsages pids delay = do
    pidTime0 <- send $ mapM (effectToMaybe . P.getProcessCPUTime) pids
    sysTime0 <- P.getCPUTime
    send $ threadDelay (fromIntegral delay)
    pidTime1 <- send $ mapM (effectToMaybe . P.getProcessCPUTime) pids
    sysTime1 <- P.getCPUTime
    let pidDiff = zipWith (\t0 t1 -> (-) <$> t1 <*> t0) pidTime0 pidTime1
        sysDiff = sysTime1 - sysTime0
    if sysDiff == 0
        then throwError "No change in CPU time."
        else pure $ ((/ fromIntegral sysDiff) . fromIntegral <$>) <$> pidDiff

getProcessMemUsage :: Members '[Error String, IO] r => PID -> Eff r Percent
getProcessMemUsage pid = do
    memTotal <- P.getMemTotal
    memPID <- P.getProcessMemUsage pid
    if memTotal == 0
        then throwError "Memory avalible is 0."
        else pure $ fromIntegral memPID / fromIntegral memTotal

getProcessMemUsages :: Members '[Error String, IO] r => [PID] -> Eff r [Maybe Percent]
getProcessMemUsages pids = do
    memTotal <- P.getMemTotal
    memPID <- send $ mapM (effectToMaybe . P.getProcessMemUsage) pids
    if memTotal == 0
        then throwError "Memory avalible is 0."
        else pure $ ((/ fromIntegral memTotal) . fromIntegral <$>) <$> memPID

getProcessName :: Members '[Error String, IO] r => PID -> Eff r String
getProcessName pid = P.pidStatComm <$> P.getPIDStat pid

getProcessTime :: Members '[Error String, IO] r => PID -> Eff r MicroSecond
getProcessTime pid = P.getProcessCPUTime pid >>= jiffyToMicroSecond

getProcessTimes :: Members '[Error String, IO] r => [PID] -> Eff r [Maybe MicroSecond]
getProcessTimes pids = do
    jiffies <- send $ mapM (effectToMaybe . P.getProcessCPUTime) pids
    mapM (mapM jiffyToMicroSecond) jiffies

getWallTime :: Members '[Error String, IO] r => Eff r MicroSecond
getWallTime = P.getCPUTime >>= jiffyToMicroSecond

jiffyToMicroSecond :: Members '[Error String, IO] r => Jiffy -> Eff r MicroSecond
jiffyToMicroSecond js = do
    tps <- fromIntegral <$> P.getClockTicksPerSecond
    pure $ (js * 1000000) `div` tps

effectToMaybe :: Eff '[Error String, IO] a -> IO (Maybe a)
effectToMaybe eff = do
    er <- runM $ runError eff
    case er of
        (Left _) -> pure Nothing
        (Right r) -> pure $ Just r
