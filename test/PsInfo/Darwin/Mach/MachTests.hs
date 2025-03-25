module PsInfo.Darwin.Mach.MachTests (tests) where

import PsInfo.Darwin.Mach
import Data.Either (isRight)
import qualified Test.HUnit as H

testGetTaskForPidInitSuccess :: H.Test
testGetTaskForPidInitSuccess = H.TestCase $ do 
    t <- getTaskForPid 1
    H.assertBool "getTaskForPid 1 (initial process) should give a value" $ isRight t

testGetTaskInfoForTaskInitSuccess :: H.Test
testGetTaskInfoForTaskInitSuccess = H.TestCase $ do 
    r <- getTaskForPid 1
    case r of 
        (Left err) -> H.assertBool "getTaskForPid 1 (init process) should give a value" False
        (Right task) -> do
            ti <- getTaskInfoForTask task
            H.assertBool "getTaskInfoForTask pid=1 (initial process) should give a value" $ isRight ti

tests :: H.Test
tests = H.TestList
    [ testGetTaskForPidInitSuccess
    , testGetTaskInfoForTaskInitSuccess
    ]
