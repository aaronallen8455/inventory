{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module DefCounts.ProcessHie
  ( DefCounter
  , DefType(..)
  , declLines
  ) where

import           Data.Map.Append.Strict (AppendMap(..))
import qualified Data.Map.Strict as M
import           Data.Monoid

import           HieTypes
import           SrcLoc

import           Utils

-- TODO standalone kind sigs
data DefType
  = Class
  | Data
  | Fam
  | Func
  | PatSyn
  | Syn
  | ClassInst
  | TyFamInst
  | ModImport
  | ExportThing
  deriving (Eq, Ord, Show)

type DefCounter =
  AppendMap DefType
            ( Sum Int -- num lines
            , Sum Int -- num occurrences
            )

-- | Counts up the different types of definitions in the given 'HieAST'.
declLines :: HieAST a -> DefCounter
declLines node
  | nodeHasAnnotation "ClsInstD" "InstDecl" node
  || nodeHasAnnotation "DerivDecl" "DerivDecl" node
  = AppendMap $ M.singleton ClassInst (numLines $ nodeSpan node, 1)

  | nodeHasAnnotation "TypeSig" "Sig" node
  = AppendMap $ M.singleton Func (numLines $ nodeSpan node, 0)

  | nodeHasAnnotation "FunBind" "HsBindLR" node
  = AppendMap $ M.singleton Func (numLines $ nodeSpan node, 1)

  | nodeHasAnnotation "ImportDecl" "ImportDecl" node
  = AppendMap $ M.singleton ModImport (numLines $ nodeSpan node, 1)

  | nodeHasAnnotation "IEName" "IEWrappedName" node
  = AppendMap $ M.singleton ExportThing (numLines $ nodeSpan node, 1)

  | otherwise = foldMap ( foldMap (foldMap tyDeclLines . identInfo)
                        . nodeIdentifiers
                        . nodeInfo )
              $ nodeChildren node

numLines :: Span -> Sum Int
numLines s = Sum $ srcSpanEndLine s - srcSpanStartLine s + 1

tyDeclLines :: ContextInfo -> DefCounter
tyDeclLines = \case
  Decl (toDefType -> Just declType) (Just srcSpan)
    -> AppendMap $ M.singleton declType (numLines srcSpan, 1)
  _ -> mempty
  where
    toDefType = \case
      FamDec    -> Just Fam
      SynDec    -> Just Syn
      DataDec   -> Just Data
      PatSynDec -> Just PatSyn
      ClassDec  -> Just Class
      InstDec   -> Just TyFamInst
      _         -> Nothing

