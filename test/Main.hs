module Main (main) where

import Test.HUnit

import PsInfo.PsInfoTests (tests)

main :: IO ()
main = do 
    _ <- runTestTT tests
    pure ()

