{-# LANGUAGE OverloadedStrings #-}
module MatchSigs.ProcessHie
  ( SigMap
  , MatchedSigs(..)
  , mkSigMap
  ) where

import qualified Data.Map.Strict as M
import           Data.Map.Append.Strict (AppendMap(..))

import           GHC.Api
import           MatchSigs.Matching (MatchedSigs(..))
import           MatchSigs.Sig (Sig, sigFingerprint, sigsFromHie)
import           Utils

type SigMap = AppendMap [Sig ()] MatchedSigs

-- | Collect all the function definitions in the 'HieAST' that have isomorphic
-- type signatures.
mkSigMap :: DynFlags -> HieAST HieTypeFix -> SigMap
mkSigMap dynFlags node =
  let renderedSigs = foldNodeChildren (nameSigRendered dynFlags) node
      sigReps = foldNodeChildren sigsFromHie node
      mkMatch n s r = (sigFingerprint r, MatchedSigs [(r, s, [n])])
      sigMatches = M.elems $ M.intersectionWithKey mkMatch renderedSigs sigReps
   in AppendMap $ M.fromListWith (<>) sigMatches

-- | Produce a 'Map' from function 'Name's to their rendered type signatures
nameSigRendered :: DynFlags -> HieAST HieTypeFix -> M.Map Name String
nameSigRendered dynFlags node
  | nodeHasAnnotation "FunBind" "HsBindLR" node
  , Just ident <- mIdent
  , Right name : _ <- M.keys . nodeIdentifiers $ getNodeInfo ident
  , let renderedTy = unwords
                   . map (renderHieType dynFlags)
                   . nodeType
                   $ getNodeInfo node
  = M.singleton name renderedTy

  | otherwise = mempty
  where
    mIdent
      | c : _ <- nodeChildren node
      -- multiple decls result in Match nodes
      , nodeHasAnnotation "Match" "Match" c
      , i : _ <- nodeChildren c
      = Just i

      | i : _ <- nodeChildren node
      = Just i

      | otherwise = Nothing
