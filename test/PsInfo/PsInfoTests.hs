module PsInfo.PsInfoTests ( tests ) where

import qualified PsInfo.Darwin.DarwinTests as D
import qualified PsInfo.Linux.LinuxTests as L

import qualified Test.HUnit as H
import System.Info ( os )

tests :: H.Test
tests = case os of
    "darwin" -> D.tests
    "linux" -> L.tests
    _ -> H.TestList []
