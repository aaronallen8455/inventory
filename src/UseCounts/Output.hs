module UseCounts.Output
  ( usageOutput
  ) where

import           Data.List
import           Data.Map.Append.Strict (AppendMap(..))
import qualified Data.Map.Strict as M
import           Data.Ord (Down(..))

import           Name
import           Outputable
import           PprColour

import           UseCounts.ProcessHie (UsageCounter, UsageCount(..))

limit :: Int
limit = 15

usageOutput :: UsageCounter -> SDoc
usageOutput (AppendMap usageCounter) =
  if length uses < limit
     then vcat uses
     else vcat [ text $ show limit ++ " Least used definitions:"
               , vcat . take limit $ reverse uses
               , text ""
               , text $ show limit ++ " Most used definitions:"
               , vcat $ take limit uses
               ]
  where
    uses = fmap (uncurry usageLine)
         . sortOn (Down . usages . snd)
         . M.toList
         $ M.filter locallyDefined usageCounter

usageLine :: Name -> UsageCount -> SDoc
usageLine name usage
  = let numUses = usages usage
        u | numUses == 1 = text "use"
          | otherwise = text "uses"
        in  nameOutput name
        $+$ nest 2 (coloured colCyanFg (intWithCommas numUses) <+> u)

nameOutput :: Name -> SDoc
nameOutput name = nameDoc <+> locDoc where
  nameDoc = coloured colYellowFg $ ppr name
  locDoc = coloured colMagentaFg . parens $ pprDefinedAt name

