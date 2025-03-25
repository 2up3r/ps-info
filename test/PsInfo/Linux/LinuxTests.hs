module PsInfo.Linux.LinuxTests ( tests ) where

import qualified PsInfo.Linux.Proc.ProcTests as Proc ( tests )

import qualified Test.HUnit as H

tests :: H.Test
tests = H.TestList 
    [ Proc.tests
    ]
