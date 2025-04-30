{-# LANGUAGE DataKinds #-}
module PsInfo.Darwin.Sysctl.SysctlTests (tests) where

import Control.Monad.Freer (runM)
import Control.Monad.Freer.Error (runError)
import Test.HUnit

import PsInfo.Darwin.Sysctl
import PsInfo.Util.Test

testSysctlSuccess :: Test
testSysctlSuccess = TestCase $ do
    let _CTL_HW = 6
        _HW_MEMSIZE = 24
    es <- runM $ runError $ getSysctl [_CTL_HW, _HW_MEMSIZE] :: IO (Either String Int)
    assertRight "getSysctl [_CTL_HW, _HW_MEMSIZE]" es
    mapM_ (assertPositive "getSysctl [_CTL_HW, _HW_MEMSIZE]") es

testSysctlFail :: Test
testSysctlFail = TestCase $ do
    let _CTL_KERN = 1
        _KERN_HOSTNAME = 10
    es <- runM $ runError $ getSysctl [_CTL_KERN, _KERN_HOSTNAME, 1] :: IO (Either String Int)
    assertLeft "getSysctl [_CTL_KERN, _KERN_HOSTNAME, 1]" es

tests :: Test
tests = TestList
    [ testSysctlSuccess
    , testSysctlFail
    ]
