{-# LANGUAGE CPP #-}
module Output
  ( printResults
  ) where

import           System.IO (stdout)

import           DefCounts.Output
import           GHC.Api (DynFlags)
import           GHC.Output
import           HieFile (Counters)
import           MatchSigs.Output
import           UseCounts.Output

printResults :: DynFlags
             -> Counters
             -> IO ()
printResults dynFlags (defCounter, usageCounter, sigDupeMap, totalLines) = do
  let output = vcat
        [ separator
        , text ""
        , keyword $ text "Duplicate Type Signatures"
        , text ""
        , sigDuplicateOutput sigDupeMap
        , text ""
        , separator
        , text ""
        , keyword $ text "Usage Totals"
        , text ""
        , usageOutput usageCounter
        , text ""
        , separator
        , text ""
        , keyword $ text "Definition Counts"
        , text ""
        , defCountOutput defCounter totalLines
        , text ""
        , separator
        ]
      separator = coloured colGreenFg $ text "********************************************************************************"

  outputSDoc dynFlags output

outputSDoc :: DynFlags -> SDoc -> IO ()
outputSDoc dynFlags sDoc = do
#if __GLASGOW_HASKELL__ >= 900
  let pprStyle = setStyleColoured True defaultUserStyle
      sDocCtx = initSDocContext dynFlags pprStyle
  printSDocLn sDocCtx PageMode stdout sDoc
#else
  let pprStyle = setStyleColoured True $ defaultUserStyle dynFlags
  printSDocLn PageMode dynFlags stdout pprStyle sDoc
#endif
