module PsInfo.Darwin.Mach.MachTests (tests) where

import Control.Monad (when)
import Data.Either (isRight)

import Control.Monad.Freer (runM)
import Control.Monad.Freer.Error (runError)
import Test.HUnit

import PsInfo.Darwin.Mach
import PsInfo.Util.Test

testGetTaskForPidInitSuccess :: Test
testGetTaskForPidInitSuccess = TestCase $ do 
    et <- runM $ runError $ getTaskForPid 1
    assertRight "getTaskForPid 1" et

testGetTaskInfoForTaskInitSuccess :: Test
testGetTaskInfoForTaskInitSuccess = TestCase $ do 
    et <- runM $ runError $ getTaskForPid 1
    assertRight "getTaskForPid 1" et
    when (isRight et) $ do
        (Right t) <- pure et
        eti <- runM $ runError $ getTaskInfoForTask t
        assertRight ("getTaskInfoForTask " ++ show t) eti

testGetHostStatistics :: Test
testGetHostStatistics = TestCase $ do
    estat <- runM $ runError getHostStatistics
    assertRight "getHostStatistics" estat
    mapM_ (assertPositive "getHostStatistics (system)" . hsSystem) estat

testGetHostSelf :: Test
testGetHostSelf = TestCase $ do
    epid <- runM $ runError getSelfPID
    assertRight "getSelfPID" epid

testGetMachTimebaseInfo :: Test
testGetMachTimebaseInfo = TestCase $ do
    emti <- runM $ runError getMachTimebaseInfo
    assertRight "getMachTimebaseInfo" emti
    mapM_ (assertPositive "getMachTimebaseInfo (denom)" . mti_denom) emti
    mapM_ (assertPositive "getMachTimebaseInfo (number)" . mti_number) emti

testGetMachAbsoluteTime :: Test
testGetMachAbsoluteTime = TestCase $ do
    mat <- getMachAbsoluteTime
    assertPositive "getMachAbsoluteTime" mat

tests :: Test
tests = TestList
    [ testGetTaskForPidInitSuccess
    , testGetTaskInfoForTaskInitSuccess
    , testGetHostStatistics
    , testGetHostSelf
    , testGetMachTimebaseInfo
    , testGetMachAbsoluteTime
    ]
