module PsInfo.Linux.LinuxTests ( tests ) where

import qualified PsInfo.Linux.Proc.ProcTests as P ( tests )
import qualified PsInfo.Linux as L

import qualified Test.HUnit as H

import Control.Monad.Freer ( runM )
import Control.Monad.Freer.Error ( runError )

testGetPIDs :: H.Test
testGetPIDs = H.TestCase $ do
    r <- (runM $ runError L.getPIDs :: IO (Either String [L.PID]))
    case r of
        (Left err) -> H.assertFailure $ "Failed to get PIDs: " ++ err
        (Right v) -> H.assertBool "Number of PIDs should be larger than zero" $ not (null v)

testGetCPUUsage :: H.Test
testGetCPUUsage = H.TestCase $ do
    r <- (runM $ runError $ L.getCPUUsage 10000 :: IO (Either String Double))
    case r of
        (Left err) -> H.assertFailure $ "Failed to get CPU usage: " ++ err
        (Right v) -> do
            H.assertBool "CPU usage in percent should not be bellow 0%" $ v >= 0
            H.assertBool "CPU usage in percent should not be above 100%" $ v <= 1

testGetProcessCPUUsage :: H.Test
testGetProcessCPUUsage = H.TestCase $ do
    r <- (runM $ runError $ L.getProcessCPUUsage 1 10000 :: IO (Either String Double))
    case r of
        (Left err) -> H.assertFailure $ "Failed to get CPU usage for pid=1: " ++ err
        (Right v) -> do
            H.assertBool "CPU usage in percent for pid=1 should not be bellow 0%" $ v >= 0
            H.assertBool "CPU usage in percent for pid=1 should not be above 100%" $ v <= 1

testGetMemUsage :: H.Test
testGetMemUsage = H.TestCase $ do
    r <- (runM $ runError $ L.getMemUsage :: IO (Either String Double))
    case r of
        (Left err) -> H.assertFailure $ "Failed to get memory usage: " ++ err
        (Right v) -> do
            H.assertBool "Memory usage in percent should not be bellow 0%" $ v >= 0
            H.assertBool "Memory usage in percent should not be above 100%" $ v <= 1

testGetProcessMemUsage :: H.Test
testGetProcessMemUsage = H.TestCase $ do
    r <- (runM $ runError $ L.getProcessMemUsage 1 :: IO (Either String Double))
    case r of
        (Left err) -> H.assertFailure $ "Failed to get memory usage for pid=1: " ++ err
        (Right v) -> do
            H.assertBool "Memory usage in percent for pid=1 should not be bellow 0%" $ v >= 0
            H.assertBool "Memory usage in percent for pid=1 should not be above 100%" $ v <= 1

tests :: H.Test
tests = H.TestList 
    [ P.tests
    , testGetPIDs
    , testGetCPUUsage
    , testGetProcessCPUUsage
    , testGetMemUsage
    , testGetProcessMemUsage
    ]
