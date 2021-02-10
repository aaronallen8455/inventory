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

data DefType
  = Class
  | Data
  | Fam
  | Func
  | PatSyn
  | Syn
  | TyClInst
  | TyFamInst
  deriving (Eq, Ord, Show)

type DefCounter =
  AppendMap DefType
            ( Sum Int
            , Sum Int
            )

-- TODO what about standalone kind sigs?
-- | Counts up the different types of definitions in the given 'HieAST'.
declLines :: HieAST a -> DefCounter
declLines node
  | nodeHasAnnotation "ClsInstD" "InstDecl" node
  = AppendMap $ M.singleton TyClInst (numLines $ nodeSpan node, 1)

  | nodeHasAnnotation "TypeSig" "Sig" node
  = AppendMap $ M.singleton Func (numLines $ nodeSpan node, 0)

  | nodeHasAnnotation "FunBind" "HsBindLR" node
  = AppendMap $ M.singleton Func (numLines $ nodeSpan node, 1)

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

