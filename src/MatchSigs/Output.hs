module MatchSigs.Output
  ( sigDuplicateOutput
  ) where

import           Data.List
import           Data.Map.Append.Strict (AppendMap(..))
import qualified Data.Map.Strict as M
import           Data.Ord (Down(..))

import           Name
import           Outputable
import           PprColour

import           MatchSigs.ProcessHie (SigMap, MatchedSigs(..))

sigDuplicateOutput :: SigMap -> SDoc
sigDuplicateOutput (AppendMap m) | null m = text "(No duplicated signatures)"
sigDuplicateOutput (AppendMap sigMap) =
  vcat . map sigLine . filter multipleNames
       . sortOn (\(_, _, names) -> Down $ length names)
       . concatMap getMatchedSigs
       $ M.elems sigMap

  where
    multipleNames (_, _, names) = length names > 1
    sigLine (_, renderedSig, names) =
      vcat
        [ coloured colCyanFg $ dcolon <+> text renderedSig
        , nest 2 $ count (length names)
        , vcat $ printName <$> names
        , text ""
        ]

    printName name =
      let nameDoc = coloured colYellowFg $ ppr name
          locDoc = coloured colMagentaFg . parens $ pprDefinedAt name
       in char '•' <+> nameDoc <+> locDoc
    count x = coloured colCyanFg (int x) <+> text "matches:"

