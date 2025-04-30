module PsInfo.Darwin.Mach.MachTests (tests) where

import Control.Monad.Freer (runM)
import Control.Monad.Freer.Error (runError)
import Test.HUnit

import PsInfo.Darwin.Mach
import PsInfo.Util.Test

testGetHostStatistics :: Test
testGetHostStatistics = TestCase $ do
    estat <- runM $ runError getHostStatistics
    assertRight "getHostStatistics" estat
    mapM_ (assertPositive "getHostStatistics (system)" . hsSystem) estat

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
    [ testGetHostStatistics
    , testGetMachTimebaseInfo
    , testGetMachAbsoluteTime
    ]
