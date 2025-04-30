{-# LANGUAGE DataKinds #-}
module PsInfo.Darwin
    ( getCPUActiveTime
    , getCPUTotalTime
    , getCPUUsage
    , getMemUsage
    , getPIDs
    , getProcessCPUUsage
    , getProcessMemUsage
    , getProcessName
    , getProcessTime
    , getWallTime
    ) where

import Control.Concurrent (threadDelay)
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
getProcessCPUUsage (PID pid) delay = do
    p0 <- pidTime <$> L.getProcTaskInfo (fromIntegral pid)
    s0 <- send M.getMachAbsoluteTime
    send $ threadDelay (fromIntegral delay)
    p1 <- pidTime <$> L.getProcTaskInfo (fromIntegral pid)
    s1 <- send M.getMachAbsoluteTime
    let ds = s1 - s0
        dp = p1 - p0
    if ds == 0
        then throwError "System time did not change."
        else pure $ fromIntegral dp / fromIntegral ds
    where
        pidTime :: L.ProcTaskInfo -> Integer
        pidTime pti = fromIntegral $ L.pti_total_user pti + L.pti_total_system pti

getProcessMemUsage :: Members '[Error String, IO] r => PID -> Eff r Percent
getProcessMemUsage (PID pid) = do
    pti <- L.getProcTaskInfo (fromIntegral pid)
    total <- S.getSysctl [6, 24] :: Members '[Error String, IO] r => Eff r CLLong
    let rss = fromIntegral $ L.pti_resident_size pti :: Integer
    if total == 0
        then throwError "Total system memory is 0."
        else pure $ fromIntegral rss / fromIntegral total

getProcessName :: Members '[Error String, IO] r => PID -> Eff r String
getProcessName (PID pid) = getFilePart <$> L.getName (fromIntegral pid)
    where
        getFilePart :: FilePath -> String
        getFilePart = reverse . takeWhile (/= '/') . reverse

getProcessTime :: Members '[Error String, IO] r => PID -> Eff r MicroSecond
getProcessTime (PID pid) = L.getProcTaskInfo (fromIntegral pid) >>= getPTITime
    where
        getPTITime :: Members '[Error String, IO] r => L.ProcTaskInfo -> Eff r MicroSecond
        getPTITime pti = do
            let ticks = fromIntegral $ L.pti_total_user pti + L.pti_total_system pti
            mti <- M.getMachTimebaseInfo
            pure $ ticks * fromIntegral (M.mti_number mti) `div` fromIntegral (M.mti_denom mti)

getWallTime :: Members '[Error String, IO] r => Eff r MicroSecond
getWallTime = do
    time <- send M.getMachAbsoluteTime
    mti <- M.getMachTimebaseInfo
    pure $ (time * fromIntegral (M.mti_number mti)) `div` fromIntegral (M.mti_denom mti)
