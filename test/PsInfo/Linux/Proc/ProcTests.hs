{-# LANGUAGE OverloadedStrings, DataKinds #-}
module PsInfo.Linux.Proc.ProcTests (tests) where

import Control.Monad.Freer.Error (runError)
import Control.Monad.Freer (runM)
import Test.HUnit

import PsInfo.Linux.Proc
import PsInfo.Util.Test
import PsInfo.Util.Types

testValidPIDs :: Test
testValidPIDs = TestCase $ do
    epids <- runM $ runError getPIDs
    assertRight "getPIDs" epids
    mapM_ (assertNotEmpty "getPIDs") epids

testValidStat :: Test
testValidStat = TestCase $ do
    estat <- runM $ runError getStat
    assertRight "getStat" estat

testValidCPUActive :: Test
testValidCPUActive = TestCase $ do
    er <- runM $ runError getCPUActive
    assertRight "getCPUActive" er
    mapM_ (assertPositive "getCPUActive") er

testValidCPUTime :: Test
testValidCPUTime = TestCase $ do
    er <- runM $ runError getCPUTime
    assertRight "getCPUTime" er
    mapM_ (assertPositive "getCPUTime") er

testValidMemInfo :: Test
testValidMemInfo = TestCase $ do
    er <- runM $ runError getMemInfo
    assertRight "getMemInfo" er

testValidMemTotal :: Test
testValidMemTotal = TestCase $ do
    er <- runM $ runError getMemTotal
    assertRight "getMemTotal" er
    mapM_ (assertPositive "getMemTotal") er

testValidMemActive :: Test
testValidMemActive = TestCase $ do
    er <- runM $ runError getMemActive
    assertRight "getMemActive" er
    mapM_ (assertPositive "getMemActive") er

testValidPIDStat :: Test
testValidPIDStat = TestCase $ do
    er <- runM $ runError $ getPIDStat (PID 1)
    assertRight "getPIDStat" er

testProcessCPUTime :: Test
testProcessCPUTime = TestCase $ do
    er <- runM $ runError $ getProcessCPUTime (PID 1)
    assertRight "getProcessCPUTime 1" er
    mapM_ (assertPositive "getProcessCPUTime 1") er

testValidPIDStatm :: Test
testValidPIDStatm = TestCase $ do
    er <- runM $ runError $ getPIDStatm (PID 1)
    assertRight "getProcessCPUTime 1" er

testProcessMemUsage :: Test
testProcessMemUsage = TestCase $ do
    er <- runM $ runError $ getProcessMemUsage (PID 1)
    assertRight "getProcessMemUsage 1" er
    mapM_ (assertNonNegative "getProcessMemUsage 1") er

testClockTicksPerSecond :: Test
testClockTicksPerSecond = TestCase $ do
    er <- runM $ runError getClockTicksPerSecond
    assertRight "getClockTicksPerSecond" er
    mapM_ (assertPositive "getClockTicksPerSecond") er

tests :: Test
tests = TestList
    [ testValidPIDs
    , testValidStat
    , testValidCPUActive
    , testValidCPUTime
    , testValidMemInfo
    , testValidMemTotal
    , testValidMemActive
    , testValidPIDStat
    , testProcessCPUTime
    , testValidPIDStatm
    , testProcessMemUsage
    , testClockTicksPerSecond
    ]
