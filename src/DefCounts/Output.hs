{-# LANGUAGE LambdaCase #-}
module DefCounts.Output
  ( defCountOutput
  ) where

import           Data.Foldable
import           Data.Map.Append.Strict (AppendMap(..))
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Text.Printf

import           Outputable
import           PprColour

import           DefCounts.ProcessHie (DefCounter, DefType(..))

defCountOutput :: DefCounter -> Sum Int -> SDoc
defCountOutput (AppendMap defCount) (Sum totalLines) =
  vcat [ header
       , vcat $ uncurry defOutput <$> M.toList defCount
       , otherCount
       , text ""
       , text "Total Lines:" <+> coloured colCyanFg (intWithCommas totalLines)
       ]
  where
    defLineTotal = getSum . fst $ fold defCount
    otherLines = totalLines - defLineTotal :: Int

    header = keyword . coloured colMagentaFg
           $ text "Type of Definition"
          $$ nest 30 (text "Num Lines")
          $$ nest 50 (text "Num Defs")
          $$ nest 70 (text "% of Total Lines")

    defOutput defType (Sum numLines, Sum numOccs)
      = pprDefType defType
     $$ nest 30 (coloured colCyanFg $ intWithCommas numLines)
     $$ nest 50 (coloured colCyanFg $ intWithCommas numOccs)
     $$ nest 70 (pprPerc $ (fromIntegral numLines :: Float) / fromIntegral totalLines * 100)

    otherCount
      = text "Miscellaneous"
     $$ nest 30 (coloured colCyanFg $ intWithCommas otherLines)
     $$ nest 70 (pprPerc $ (fromIntegral otherLines :: Float) / fromIntegral totalLines * 100)

    pprPerc = coloured colCyanFg . text . printf "%.1f%%"

pprDefType :: DefType -> SDoc
pprDefType = \case
  Func        -> text "Function"
  Fam         -> text "Type/Data Family"
  Data        -> text "Data"
  Class       -> text "Type Class"
  TyFamInst   -> text "Type/Data Family Instance"
  TyClInst    -> text "Type Class Instance"
  Syn         -> text "Type Synonym"
  PatSyn      -> text "Pattern Synonym"
  ModImport   -> text "Import"
  ExportThing -> text "Export"

