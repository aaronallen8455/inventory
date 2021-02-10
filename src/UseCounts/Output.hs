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

-- TODO only print top 20 and bottom 20 if there are more than 20

usageOutput :: UsageCounter -> SDoc
usageOutput (AppendMap usageCounter) = vcat uses
  where
    uses = fmap (uncurry usageLine)
         . sortOn (Down . usages . snd)
         $ M.toList usageCounter

usageLine :: Name -> UsageCount -> SDoc
usageLine name usage
  | not $ locallyDefined usage = empty
  | let numUses = usages usage
        u | numUses == 1 = text "use"
          | otherwise = text "uses"
  = nameOutput name
  $+$ nest 2 (coloured colCyanFg (intWithCommas numUses) <+> u)

nameOutput :: Name -> SDoc
nameOutput name = nameDoc <+> locDoc where
  nameDoc = coloured colYellowFg $ ppr name
  locDoc = coloured colMagentaFg . parens $ pprDefinedAt name

