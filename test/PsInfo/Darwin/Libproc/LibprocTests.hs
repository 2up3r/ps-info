module PsInfo.Darwin.Libproc.LibprocTests (tests) where

import PsInfo.Darwin.Libproc as L

import qualified Test.HUnit as H

import Control.Monad.Freer (runM)
import Control.Monad.Freer.Error (runError)

testGetListPidsSuccess :: H.Test
testGetListPidsSuccess = H.TestCase $ do
    epids <- runM $ runError L.getListPids
    case epids of 
        (Left err) -> H.assertFailure $ "getListPids should return items: " ++ err 
        (Right pids) -> H.assertBool "Length of getListPids should not be zero" $ not (null pids)

testGetProcTaskInfoSucess :: H.Test
testGetProcTaskInfoSucess = H.TestCase $ do
    epti <- (runM $ runError $ L.getProcTaskInfo 1 :: IO (Either String L.ProcTaskInfo))
    case epti of
        (Left err) -> H.assertFailure $ "getProcTaskInfo 1 (initial process) should give a value: " ++ err
        (Right pti) -> do
            H.assertBool ("getProcTaskInfo 1 (initial process) should give a values (probably missing permissions): " ++ show pti) $ L.pti_total_user pti /= 0
            H.assertBool ("getProcTaskInfo 1 (initial process) should not have incorrect values (probably missing permissions): " ++ show pti) $ pti_syscalls_mach pti == 0

tests :: H.Test
tests = H.TestList 
    [ testGetListPidsSuccess
    , testGetProcTaskInfoSucess
    ]
