module PsInfo.PsInfoTests (tests) where

import System.Info (os)
import Test.HUnit

import qualified PsInfo.Darwin.DarwinTests as D
import qualified PsInfo.Linux.LinuxTests as L

tests :: Test
tests = case os of
    "darwin" -> D.tests
    "linux" -> L.tests
    _ -> TestList []
