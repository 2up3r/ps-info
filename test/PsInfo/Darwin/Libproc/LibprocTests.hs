module PsInfo.Darwin.Libproc.LibprocTests (tests) where

import Control.Monad.Freer (runM)
import Control.Monad.Freer.Error (runError)
import Test.HUnit

import PsInfo.Darwin.Libproc
import PsInfo.Util.Test

testGetListPidsSuccess :: Test
testGetListPidsSuccess = TestCase $ do
    epids <- runM $ runError getListPids
    assertRight "getListPids" epids
    mapM_ (assertNotEmpty "getListPids") epids

testGetProcTaskInfoSuccess :: Test
testGetProcTaskInfoSuccess = TestCase $ do
    epti <- (runM $ runError $ getProcTaskInfo 1 :: IO (Either String ProcTaskInfo))
    assertRight "getProcTaskInfo 1" epti
    mapM_ (assertPositive "getProcTaskInfo 1 (total system time)" . pti_total_system) epti

testGetNameSuccess :: Test
testGetNameSuccess = TestCase $ do
    ename <- runM $ runError $ getName 1
    assertRight "getName 1" ename
    mapM_ (assertNotEmpty "getName 1") ename

tests :: Test
tests = TestList 
    [ testGetListPidsSuccess
    , testGetProcTaskInfoSuccess
    , testGetNameSuccess
    ]
