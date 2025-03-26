{-# LANGUAGE OverloadedStrings, DataKinds, FlexibleContexts, GADTs #-}
module PsInfo.Linux.Proc.ProcTests ( tests ) where

import qualified PsInfo.Linux.Proc as P

import qualified Test.HUnit as H

import Data.Either ( isRight, fromLeft )
import Control.Monad.Freer.Error ( runError )
import Control.Monad.Freer ( runM )

testValidPIDs :: H.Test
testValidPIDs = H.TestCase $ do
    r <- runM $ runError P.getPIDs :: IO (Either String [P.PID])
    case r of
        (Left err) -> H.assertFailure $ "/proc should be accessable/correct: " ++ err
        (Right pids) -> H.assertBool "There should be some active processes." $ not (null pids)

testValidStat :: H.Test
testValidStat = H.TestCase $ do
    r <- runM $ runError P.getStat :: IO (Either String P.Stat)
    H.assertBool ("/proc/stat should be accessable/correct: " ++ fromLeft "" r) $ isRight r

testValidMemInfo :: H.Test
testValidMemInfo = H.TestCase $ do
    r <- runM $ runError P.getMemInfo :: IO (Either String P.MemInfo)
    H.assertBool ("/proc/meminfo should be accessable/correct: " ++ fromLeft "" r) $ isRight r

testValidPIDStat :: H.Test
testValidPIDStat = H.TestCase $ do
    r <- runM $ runError $ P.getPIDStat 1 :: IO (Either String P.PIDStat)
    H.assertBool ("/proc/1/stat should be accessable/correct: " ++ fromLeft "" r) $ isRight r

testValidPIDStatm :: H.Test
testValidPIDStatm = H.TestCase $ do
    r <- runM $ runError $ P.getPIDStatm 1 :: IO (Either String P.PIDStatm)
    H.assertBool ("/proc/1/statm should be accessable/correct: " ++ fromLeft "" r) $ isRight r

tests :: H.Test
tests = H.TestList
    [ testValidPIDs
    , testValidStat
    , testValidMemInfo
    , testValidPIDStat
    , testValidPIDStatm
    ]
