module PsInfo.Darwin.Mach.MachTests (tests) where

import PsInfo.Darwin.Mach
import qualified Test.HUnit as H
import Control.Monad.Freer (runM)
import Control.Monad.Freer.Error (runError)

testGetTaskForPidInitSuccess :: H.Test
testGetTaskForPidInitSuccess = H.TestCase $ do 
    et <- runM $ runError $ getTaskForPid 1
    case et of
        (Left err) -> H.assertFailure $ "getTaskForPid 1 (initial process) should give a value: " ++ err
        (Right _) -> pure ()

testGetTaskInfoForTaskInitSuccess :: H.Test
testGetTaskInfoForTaskInitSuccess = H.TestCase $ do 
    et <- runM $ runError $ getTaskForPid 1
    case et of 
        (Left err) -> H.assertFailure $ "getTaskForPid 1 (init process) should give a value: " ++ err
        (Right t) -> do
            eti <- runM $ runError $ getTaskInfoForTask t
            case eti of
                (Left err) -> H.assertFailure $ "getTaskInfoForTask pid=1 (initial process) should give a value: " ++ err
                (Right _) -> pure ()

tests :: H.Test
tests = H.TestList
    [ testGetTaskForPidInitSuccess
    , testGetTaskInfoForTaskInitSuccess
    ]
