module PsInfo.Util.Test
    ( assertRight
    , assertLeft
    , assertNonNegative
    , assertPositive
    , assertBetween
    , assertNotEmpty
    ) where

import Data.Either (isLeft, isRight)

import Test.HUnit

assertRight :: String -> Either String a -> Assertion
assertRight command a = assertBool
    (command ++ " should not fail. Failed with error: " ++ either show (const "") a)
    (isRight a)

assertLeft :: Show a => String -> Either e a -> Assertion
assertLeft command a = assertBool
    (command ++ " should fail. Succeeded with value: " ++ either (const "") show a)
    (isLeft a)

assertNonNegative :: (Num a, Show a, Ord a) => String -> a -> Assertion
assertNonNegative command a = assertBool
    (command ++ " should give a non-negative value, was: " ++ show a)
    (a >= 0)

assertPositive :: (Num a, Show a, Ord a) => String -> a -> Assertion
assertPositive command a = assertBool
    (command ++ " should give a positive value, was: " ++ show a)
    (a > 0)

assertBetween :: (Show a, Ord a) => String -> a -> a -> a -> Assertion
assertBetween command mn mx v = assertBool
    (command ++ " should give a value between " ++ show mn ++ " and " ++ show mx ++ ", was: " ++ show v)
    (mn <= v && v <= mx)

assertNotEmpty :: Foldable m => String -> m a -> Assertion
assertNotEmpty command xs = assertBool
    (command ++ " should give at least one element")
    (not (null xs))
