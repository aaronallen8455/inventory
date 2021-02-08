module Output
  ( printResults
  ) where

import           Data.Monoid

import           DynFlags (unsafeGlobalDynFlags)
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

printResults :: (DefCounter, UsageCounter, SigMap, Sum Int)
             -> IO ()
printResults (defCounter, usageCounter, sigDupeMap, totalLines) =
  let output = vcat
        [ separator
        , text ""
        , keyword $ text "Definition Counts"
        , text ""
        , defCountOutput defCounter totalLines
        , text ""
        , separator
        , text ""
        , keyword $ text "Usage Totals"
        , text ""
        , usageOutput usageCounter
        , text ""
        , separator
        , text ""
        , keyword $ text "Duplicate Type Signatures"
        , text ""
        , sigDuplicateOutput sigDupeMap
        ]
      dynFlags = unsafeGlobalDynFlags
      pprStyle = setStyleColoured True $ defaultUserStyle dynFlags

      separator = coloured colGreenFg $ text "********************************************************************************"

   in printSDocLn PageMode dynFlags stdout pprStyle output

