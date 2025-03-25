module PsInfo.Darwin.DarwinTests (tests) where

import qualified PsInfo.Darwin.Libproc.LibprocTests as L (tests)
import qualified PsInfo.Darwin.Mach.MachTests as M (tests)
import qualified PsInfo.Darwin.Rusage.RusageTests as R (tests)
import qualified PsInfo.Darwin.Sysctl.SysctlTests as S (tests)

import qualified Test.HUnit as H

tests :: H.Test
tests = H.TestList 
    [ L.tests
    , M.tests
    , R.tests
    , S.tests
    ]
