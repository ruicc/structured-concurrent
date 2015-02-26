module Main where

import           Test.Tasty
--import           Test.Tasty.HUnit as HU
import           Test.Tasty.QuickCheck as QC
import           System.Exit (exitFailure, exitSuccess)

import           Control.Concurrent.Structured


main :: IO ()
main = do
    putStrLn "This test is always failed."
    exitFailure
