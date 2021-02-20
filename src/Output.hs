module Output
  ( printResults
  ) where

import           DynFlags
import           Outputable
import           PprColour
import           Pretty (Mode(PageMode))
import           System.IO (stdout)

import           DefCounts.Output
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
      pprStyle = setStyleColoured True $ defaultUserStyle dynFlags

      separator = coloured colGreenFg $ text "********************************************************************************"

  printSDocLn PageMode dynFlags stdout pprStyle output

