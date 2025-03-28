module PsInfo.Darwin.Rusage.RusageTests (tests) where

import qualified Test.HUnit as H

import Control.Monad.Freer (runM)
import Control.Monad.Freer.Error (runError)
import PsInfo.Darwin.Rusage

testGetRusageSelf :: H.Test
testGetRusageSelf = H.TestCase $ do 
    eru <- runM $ runError $ getRusage _RUSAGE_SELF
    case eru of
        (Left err) -> H.assertFailure $ "getRusage 0 (RUSAGE_SELF) should not fail" ++ err
        (Right _) -> pure ()

testGetRusageChildren :: H.Test
testGetRusageChildren = H.TestCase $ do 
    eru <- runM $ runError $ getRusage _RUSAGE_CHILDREN
    case eru of
        (Left err) -> H.assertFailure $ "getRusage -1 (RUSAGE_CHILDREN) should not fail" ++ err
        (Right _) -> pure()

testGetRusageFailure :: H.Test
testGetRusageFailure = H.TestCase $ do 
    eru <- runM $ runError $ getRusage 1
    case eru of
        (Left err) -> H.assertFailure $ "getRusage 1 should fail" ++ err
        (Right _) -> pure ()

tests :: H.Test
tests = H.TestList 
    [ testGetRusageSelf
    , testGetRusageChildren
    , testGetRusageFailure
    ]
