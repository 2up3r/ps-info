module PsInfo.Linux.LinuxTests (tests) where

import Control.Monad.Freer (runM)
import Control.Monad.Freer.Error (runError)
import Test.HUnit

import PsInfo.Linux
import PsInfo.Util.Test
import PsInfo.Util.Types (PID (..))
import qualified PsInfo.Linux.Proc.ProcTests as P (tests)

testGetPIDs :: Test
testGetPIDs = TestCase $ do
    er <- runM $ runError getPIDs
    assertRight "getPIDs" er
    mapM_ (assertNotEmpty "getPIDs") er

testGetCPUUsage :: Test
testGetCPUUsage = TestCase $ do
    er <- runM $ runError $ getCPUUsage 10000
    assertRight "getCPUUsage 10ms" er
    mapM_ (assertBetween "getCPUUsage 10ms" 0 1) er
    mapM_ (assertNonNegative "getCPUUsage 10ms") er

testGetProcessCPUUsage :: Test
testGetProcessCPUUsage = TestCase $ do
    er <- runM $ runError $ getProcessCPUUsage (PID 1) 10000
    assertRight "getProcessCPUUsage 1 10ms" er
    mapM_ (assertBetween "getProcessCPUUsage 1 10ms" 0 1) er

testGetMemUsage :: Test
testGetMemUsage = TestCase $ do
    er <- runM $ runError getMemUsage
    assertRight "getMemUsage" er
    mapM_ (assertBetween "getMemUsage" 0 1) er
    mapM_ (assertNonNegative "getMemUsage") er

testGetProcessMemUsage :: Test
testGetProcessMemUsage = TestCase $ do
    er <- runM $ runError $ getProcessMemUsage (PID 1)
    assertRight "getProcessMemUsage 1" er
    mapM_ (assertBetween "getProcessMemUsage 1" 0 1) er

testGetProcessTime :: Test
testGetProcessTime = TestCase $ do
    etime <- runM $ runError $ getProcessTime (PID 1)
    assertRight "getProcessTime 1" etime
    mapM_ (assertNonNegative "getProcessTime 1") etime

testGetProcessName :: Test
testGetProcessName = TestCase $ do
    ename <- runM $ runError $ getProcessName (PID 1)
    assertRight "getProcessName 1" ename
    mapM_ (assertNotEmpty "getProcessTime 1") ename

tests :: Test
tests = TestList
    [ P.tests
    , testGetPIDs
    , testGetCPUUsage
    , testGetProcessCPUUsage
    , testGetMemUsage
    , testGetProcessMemUsage
    , testGetProcessTime
    , testGetProcessName
    ]
