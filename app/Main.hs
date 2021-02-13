module Main where

import           GHC.DynFlags
import           HieFile
import           Output

main :: IO ()
main = do
  dynFlags <- baseDynFlags
  getCounters dynFlags >>= printResults dynFlags
