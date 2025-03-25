module PsInfo.Darwin.Libproc.LibprocTests (tests) where

import PsInfo.Darwin.Libproc
import qualified Test.HUnit as H
import Data.Either (isRight)

testGetListPidsSuccess :: H.Test
testGetListPidsSuccess = H.TestCase $ do
    pids <- getListPids
    H.assertBool "getListPids should return items" $ isRight pids

testGetProcTaskInfoSucess :: H.Test
testGetProcTaskInfoSucess = H.TestCase $ do
    pti <- getProcTaskInfo 1
    putStrLn $ show pti
    H.assertBool "getProcTaskInfo 1 (initial process) should give a value" $ isRight pti

tests :: H.Test
tests = H.TestList 
    [ testGetListPidsSuccess
    , testGetProcTaskInfoSucess
    ]
