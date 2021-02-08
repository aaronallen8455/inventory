module Main where

import           HieFile
import           Output

main :: IO ()
main = do
  getCounters >>= printResults
