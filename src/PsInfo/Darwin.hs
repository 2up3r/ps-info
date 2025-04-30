{-# LANGUAGE DataKinds #-}
module PsInfo.Darwin
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

import Control.Concurrent (threadDelay)
import Control.Monad (forM)
import Foreign.C (CLLong)

import Control.Monad.Freer (Eff, Members, send)
import Control.Monad.Freer.Error (Error, throwError)

import PsInfo.Util.Types (Percent, PID (..), MicroSecond)
import qualified PsInfo.Darwin.Libproc as L
import qualified PsInfo.Darwin.Mach as M
import qualified PsInfo.Darwin.Sysctl as S

getCPUActiveTime :: Members '[Error String, IO] r => Eff r MicroSecond
getCPUActiveTime = do
    s <- M.getHostStatistics
    pure $ fromIntegral $ M.hsUser s + M.hsSystem s + M.hsNice s

getCPUTotalTime :: Members '[Error String, IO] r => Eff r MicroSecond
getCPUTotalTime = do
    s <- M.getHostStatistics
    pure $ fromIntegral $ M.hsUser s + M.hsSystem s + M.hsNice s + M.hsIdle s

getCPUUsage :: Members '[Error String, IO] r => MicroSecond -> Eff r Percent
getCPUUsage delay = do
    a0 <- getCPUActiveTime
    t0 <- getCPUTotalTime
    send $ threadDelay (fromIntegral delay)
    a1 <- getCPUActiveTime
    t1 <- getCPUTotalTime
    let da = a1 - a0
        dt = t1 - t0
    if dt == 0
        then throwError "getCPUUsage failed! Total system time was 0."
        else pure $ fromIntegral da / fromIntegral dt

getMemUsage :: Members '[Error String, IO] r => Eff r Percent
getMemUsage = do
    mem <- S.getSysctl [6, 5] :: Members '[Error String, IO] r => Eff r CLLong
    tot <- S.getSysctl [6, 24] :: Members '[Error String, IO] r => Eff r CLLong
    if tot == 0
        then throwError "Total memory is 0."
        else pure $ fromIntegral mem / fromIntegral tot

getPIDs :: Members '[Error String, IO] r => Eff r [PID]
getPIDs = (PID . fromIntegral <$>) <$> L.getListPids

getProcessCPUUsage :: Members '[Error String, IO] r => PID -> MicroSecond -> Eff r Percent
getProcessCPUUsage pid delay = do
    usages <- getProcessCPUUsages [pid] delay
    if null usages
        then throwError "Error in getProcessCPUUsage: could not find PID."
        else case head usages of
            Nothing -> throwError "Error in getProcessCPUUsage: could not find PID."
            (Just usage) -> pure usage

getProcessCPUUsages :: Members '[Error String, IO] r => [PID] -> MicroSecond -> Eff r [Maybe Percent]
getProcessCPUUsages pids delay = do
    pti0s <- L.getProcTaskInfos pids
    let p0s = (pidTime <$>) <$> pti0s
    s0 <- send M.getMachAbsoluteTime
    send $ threadDelay (fromIntegral delay)
    pti1s <- L.getProcTaskInfos pids
    let p1s = (pidTime <$>) <$> pti1s
    s1 <- send M.getMachAbsoluteTime
    let ds = s1 - s0
        dps = (\(p0, p1) -> (-) <$> p1 <*> p0) <$> zip p0s p1s
    if ds == 0
        then throwError "System time did not change."
        else pure $ ((/ fromIntegral ds) . fromIntegral <$>) <$> dps
    where
        pidTime :: L.ProcTaskInfo -> Integer
        pidTime pti = fromIntegral $ L.pti_total_user pti + L.pti_total_system pti

getProcessMemUsage ::  Members '[Error String, IO] r => PID -> Eff r Percent
getProcessMemUsage pid = do
    usages <- getProcessMemUsages [pid]
    if null usages
        then throwError "Error in getProcessMemUsage: Could not find PID."
        else case head usages of
            Nothing -> throwError "Error in getProcessMemUsage: Could not find PID."
            (Just usage) -> pure usage

getProcessMemUsages :: Members '[Error String, IO] r => [PID] -> Eff r [Maybe Percent]
getProcessMemUsages pids = do
    ptis <- L.getProcTaskInfos pids
    total <- S.getSysctl [6, 24] :: Members '[Error String, IO] r => Eff r CLLong
    let rsss = (fromIntegral . L.pti_resident_size <$>) <$> ptis :: [Maybe Integer]
    if total == 0
        then throwError "Total system memory is 0."
        else pure $ ((/ fromIntegral total) . fromIntegral <$>) <$> rsss

getProcessName :: Members '[Error String, IO] r => PID -> Eff r String
getProcessName (PID pid) = getFilePart <$> L.getName (fromIntegral pid)
    where
        getFilePart :: FilePath -> String
        getFilePart = reverse . takeWhile (/= '/') . reverse

getProcessTime :: Members '[Error String, IO] r => PID -> Eff r MicroSecond
getProcessTime pid = do
    times <- getProcessTimes [pid]
    if null times
        then throwError "Error in getProcessTime: could not find PID."
        else case head times of
            Nothing -> throwError "Error in getProcessTime: could not find PID."
            (Just time) -> pure time

getProcessTimes :: Members '[Error String, IO] r => [PID] -> Eff r [Maybe MicroSecond]
getProcessTimes [] = pure []
getProcessTimes pids = do
    ptis <- L.getProcTaskInfos pids
    forM ptis getPTITime
    where
        getPTITime :: Members '[Error String, IO] r => Maybe L.ProcTaskInfo -> Eff r (Maybe MicroSecond)
        getPTITime Nothing = pure Nothing
        getPTITime (Just pti) = do
            let ticks = fromIntegral $ L.pti_total_user pti + L.pti_total_system pti
            mti <- M.getMachTimebaseInfo
            pure $ Just $ ticks * fromIntegral (M.mti_number mti) `div` fromIntegral (M.mti_denom mti)

getWallTime :: Members '[Error String, IO] r => Eff r MicroSecond
getWallTime = do
    time <- send M.getMachAbsoluteTime
    mti <- M.getMachTimebaseInfo
    pure $ (time * fromIntegral (M.mti_number mti)) `div` fromIntegral (M.mti_denom mti)
