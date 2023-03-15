{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module DefCounts.ProcessHie
  ( DefCounter
  , DefType(..)
  , declLines
  ) where

import qualified Data.Array as A
import qualified Data.ByteString as BS
import           Data.Map.Append.Strict (AppendMap(..))
import qualified Data.Map.Strict as M
import           Data.Monoid

import           GHC.Api
import           Utils

-- TODO standalone kind sigs
data DefType
  = Func
  | Data
  | Newtype
  | Class
  | ClassInst
  | Fam
  | TyFamInst
  | Syn
  | PatSyn
  | ModImport
  | ExportThing
  deriving (Eq, Ord, Show)

type DefCounter =
  AppendMap DefType
            ( Sum Int -- num lines
            , Sum Int -- num occurrences
            )

-- | Supports indexing into the source code by line number
type SourceCode = A.Array Int BS.ByteString

-- | Counts up the different types of definitions in the given 'HieAST'.
declLines :: SourceCode -> HieAST a -> DefCounter
declLines src node
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

  | nodeHasAnnotation "IEThingAll" "IE" node
  = AppendMap $ M.singleton ExportThing (numLines $ nodeSpan node, 1)

  | nodeHasAnnotation "IEPattern" "IEWrappedName" node
  = AppendMap $ M.singleton ExportThing (numLines $ nodeSpan node, 1)

  | nodeHasAnnotation "IEVar" "IE" node
  = AppendMap $ M.singleton ExportThing (numLines $ nodeSpan node, 1)

  | otherwise = foldNodeChildren (tyDeclLines src) node

numLines :: Span -> Sum Int
numLines s = Sum $ srcSpanEndLine s - srcSpanStartLine s + 1

tyDeclLines :: SourceCode -> HieAST a -> DefCounter
tyDeclLines src node = fromCurrentNode <> fromChildren
  where
    fromCurrentNode =
      foldMap (foldMap go . identInfo) . nodeIdentifiers $ getNodeInfo node
    fromChildren = foldNodeChildren (tyDeclLines src) node

    go = \case
      Decl declTy (Just srcSpan)
        | Just defTy <- toDefType srcSpan declTy
        -> AppendMap $ M.singleton defTy (numLines srcSpan, 1)
      _ -> mempty
      where
        toDefType srcSpan = \case
          FamDec           -> Just Fam
          SynDec           -> Just Syn
          DataDec
            | isNewtypeDec -> Just Newtype
            | otherwise    -> Just Data
          PatSynDec        -> Just PatSyn
          ClassDec         -> Just Class
          InstDec          -> Just TyFamInst
          _                -> Nothing
          where
            isNewtypeDec =
              let ln = srcSpanStartLine srcSpan - 1
                  col = srcSpanStartCol srcSpan - 1
                  (lBnd, uBnd) = A.bounds src
               in ln >= lBnd && ln <= uBnd
               && "newtype" == (BS.take 7 . BS.drop col $ src A.! ln)
