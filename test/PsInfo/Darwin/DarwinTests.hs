module PsInfo.Darwin.DarwinTests (tests) where

import Control.Monad.Freer (runM)
import Control.Monad.Freer.Error (runError)
import Test.HUnit

import PsInfo.Darwin
import PsInfo.Util.Test
import PsInfo.Util.Types (PID (..))
import qualified PsInfo.Darwin.Libproc.LibprocTests as L (tests)
import qualified PsInfo.Darwin.Mach.MachTests as M (tests)
import qualified PsInfo.Darwin.Rusage.RusageTests as R (tests)
import qualified PsInfo.Darwin.Sysctl.SysctlTests as S (tests)

testGetCPUActiveTime :: Test
testGetCPUActiveTime = TestCase $ do
    eactive <- runM $ runError getCPUActiveTime
    assertRight "getCPUActiveTime" eactive
    mapM_ (assertPositive "getCPUActiveTime") eactive

testGetCPUTotalTime :: Test
testGetCPUTotalTime = TestCase $ do
    etotal <- runM $ runError getCPUTotalTime
    assertRight "getCPUTotalTime" etotal
    mapM_ (assertPositive "getCPUTotalTime") etotal

testGetCPUUsage :: Test
testGetCPUUsage = TestCase $ do
    eusage <- runM $ runError $ getCPUUsage 1000000
    assertRight "getCPUUsage 1s" eusage
    mapM_ (assertBetween "getCPUUsage 1s" 0 1) eusage
    mapM_ (assertPositive "getCPUUsage 1s") eusage

testGetMemUsage :: Test
testGetMemUsage = TestCase $ do
    eusage <- runM $ runError getMemUsage
    assertRight "getMemUsage" eusage
    mapM_ (assertBetween "getMemUsage" 0 1) eusage
    mapM_ (assertPositive "getMemUsage") eusage

testGetPIDs :: Test
testGetPIDs = TestCase $ do
    epids <- runM $ runError getPIDs
    assertRight "getPIDs" epids
    mapM_ (assertNotEmpty "getPIDs") epids

testGetProcessCPUUsage :: Test
testGetProcessCPUUsage = TestCase $ do
    eusage <- runM $ runError $ getProcessCPUUsage (PID 1) 10000
    assertRight "getProcessCPUUsage 1 10ms" eusage
    mapM_ (assertBetween "getProcessCPUUsage 1 10ms" 0 1) eusage

testGetProcessCPUUsages :: Test
testGetProcessCPUUsages = TestCase $ do
    eusages <- runM $ runError $ getProcessCPUUsages [PID 0, PID 1] 10000
    assertRight "getProcessCPUUsage 1 10ms" eusages
    mapM_ (mapM $ mapM $ assertBetween "getProcessCPUUsage 1 10ms" 0 1) eusages

testGetProcessMemUsage :: Test
testGetProcessMemUsage = TestCase $ do
    eusage <- runM $ runError $ getProcessMemUsage (PID 1)
    assertRight "getProcessMemUsage 1" eusage
    mapM_ (assertBetween "getProcessMemUsage 1" 0 1) eusage

testGetProcessMemUsages :: Test
testGetProcessMemUsages = TestCase $ do
    eusages <- runM $ runError $ getProcessMemUsages [PID 0, PID 1]
    assertRight "getProcessMemUsage 1" eusages
    mapM_ (mapM $ mapM $ assertBetween "getProcessMemUsage 1" 0 1) eusages

testGetProcessName :: Test
testGetProcessName = TestCase $ do
    ename <- runM $ runError $ getProcessName (PID 1)
    assertRight "getProcessName 1" ename
    mapM_ (assertNotEmpty "getProcessName 1") ename

testGetProcessTime :: Test
testGetProcessTime = TestCase $ do
    etime <- runM $ runError $ getProcessTime (PID 1)
    assertRight "getProcessTime 1" etime
    mapM_ (assertPositive "getProcessTime 1") etime

testGetProcessTimes :: Test
testGetProcessTimes = TestCase $ do
    etimes <- runM $ runError $ getProcessTimes [PID 0, PID 1]
    assertRight "getProcessTime 1" etimes
    mapM_ (mapM $ mapM $ assertPositive "getProcessTime 1") etimes

testGetWallTime :: Test
testGetWallTime = TestCase $ do
    ewall <- runM $ runError getWallTime
    assertRight "getWallTime" ewall
    mapM_ (assertPositive "getWallTime") ewall

tests :: Test
tests = TestList 
    [ L.tests
    , M.tests
    , R.tests
    , S.tests
    , testGetCPUActiveTime
    , testGetCPUTotalTime
    , testGetCPUUsage
    , testGetMemUsage
    , testGetPIDs
    , testGetProcessCPUUsage
    , testGetProcessCPUUsages
    , testGetProcessMemUsage
    , testGetProcessMemUsages
    , testGetProcessName
    , testGetProcessTime
    , testGetProcessTimes
    , testGetWallTime
    ]
