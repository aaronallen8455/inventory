module Output
  ( printResults
  ) where

import           Data.Monoid

import           DynFlags
import           Outputable
import           PprColour
import           Pretty (Mode(PageMode))
import           System.IO (stdout)

import           DefCounts.Output
import           DefCounts.ProcessHie
import           MatchSigs.Output
import           MatchSigs.ProcessHie
import           UseCounts.Output
import           UseCounts.ProcessHie (UsageCounter)

printResults :: DynFlags
             -> (DefCounter, UsageCounter, SigMap, Sum Int)
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

