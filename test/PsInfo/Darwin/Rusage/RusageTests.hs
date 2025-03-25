module PsInfo.Darwin.Rusage.RusageTests (tests) where

import PsInfo.Darwin.Rusage
import Data.Either (isRight, isLeft)
import qualified Test.HUnit as H

testGetRusageSelf :: H.Test
testGetRusageSelf = H.TestCase $ do 
    ru <- getRusage _RUSAGE_SELF
    H.assertBool "getRusage 0 (RUSAGE_SELF) should not fail" $ isRight ru

testGetRusageChildren :: H.Test
testGetRusageChildren = H.TestCase $ do 
    ru <- getRusage _RUSAGE_CHILDREN
    H.assertBool "getRusage -1 (RUSAGE_CHILDREN) should not fail" $ isRight ru

testGetRusageFailure :: H.Test
testGetRusageFailure = H.TestCase $ do 
    ru <- getRusage 1
    H.assertBool "getRusage 1 should fail" $ isLeft ru

tests :: H.Test
tests = H.TestList 
    [ testGetRusageSelf
    , testGetRusageChildren
    , testGetRusageFailure
    ]
