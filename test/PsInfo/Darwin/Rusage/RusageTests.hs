module PsInfo.Darwin.Rusage.RusageTests (tests) where

import Control.Monad.Freer (runM)
import Control.Monad.Freer.Error (runError)
import Test.HUnit

import PsInfo.Darwin.Rusage
import PsInfo.Util.Test

testGetRusageSelf :: Test
testGetRusageSelf = TestCase $ do 
    eru <- runM $ runError $ getRusage _RUSAGE_SELF
    assertRight "getRusage RUSAGE_SELF" eru

testGetRusageChildren :: Test
testGetRusageChildren = TestCase $ do 
    eru <- runM $ runError $ getRusage _RUSAGE_CHILDREN
    assertRight "getRusage RUSAGE_CHILDREN" eru

testGetRusageFailure :: Test
testGetRusageFailure = TestCase $ do 
    eru <- runM $ runError $ getRusage 2 :: IO (Either String RUsage)
    assertLeft "getRusage 2" eru

tests :: Test
tests = TestList 
    [ testGetRusageSelf
    , testGetRusageChildren
    , testGetRusageFailure
    ]
