module MatchSigs.Output
  ( sigDuplicateOutput
  ) where

import           Data.Map.Append.Strict (AppendMap(..))
import qualified Data.Map.Strict as M

import           GHC.Api hiding (count)
import           GHC.Output
import           MatchSigs.ProcessHie (SigMap, MatchedSigs(..))

sigDuplicateOutput :: SigMap -> SDoc
sigDuplicateOutput (AppendMap sigMap) =
  if null outputLines
     then text "(No duplicated signatures)"
     else vcat outputLines

  where
    outputLines = map sigLine . filter multipleNames
                . concatMap getMatchedSigs
                $ M.elems sigMap

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

