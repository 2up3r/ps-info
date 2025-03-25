module PsInfo.Darwin.Sysctl.SysctlTests (tests) where

import PsInfo.Darwin.Sysctl

import Data.Either (isRight, isLeft)
import qualified Test.HUnit as H

testSysctlSuccess :: H.Test
testSysctlSuccess = H.TestCase $ do 
    r <- getSysctl [1, 1]
    H.assertBool "sysctl 1,1 should give a value" $ isRight r

testSysctlFail :: H.Test
testSysctlFail = H.TestCase $ do
    r <- getSysctl [1, 1, 1]
    H.assertBool "sysctl 1,1 should give a value" $ isLeft r

tests :: H.Test
tests = H.TestList 
    [ testSysctlSuccess
    , testSysctlFail
    ]
