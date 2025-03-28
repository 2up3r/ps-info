module PsInfo.Darwin.Sysctl.SysctlTests (tests) where

import PsInfo.Darwin.Sysctl

import qualified Test.HUnit as H

import Control.Monad.Freer (runM)
import Control.Monad.Freer.Error (runError)

testSysctlSuccess :: H.Test
testSysctlSuccess = H.TestCase $ do 
    es <- runM $ runError $ getSysctl [1, 1]
    case es of
        (Left err) -> H.assertFailure $ "sysctl 1,1 should give a value: " ++ err
        (Right _) -> pure ()

testSysctlFail :: H.Test
testSysctlFail = H.TestCase $ do
    es <- (runM $ runError $ getSysctl [1, 1, 1] :: IO (Either String String))
    case es of
        (Left _) -> pure ()
        (Right s) -> H.assertFailure $ "sysctl 1,1,1 should not give a value: " ++ show s

tests :: H.Test
tests = H.TestList 
    [ testSysctlSuccess
    , testSysctlFail
    ]
