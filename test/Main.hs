module Main (main) where

import PsInfo.PsInfoTests
import qualified Test.HUnit as H


main :: IO ()
main = do 
    counts <- H.runTestTT tests
    print counts

