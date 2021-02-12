{-# LANGUAGE OverloadedStrings #-}
module MatchSigs.ProcessHie
  ( SigMap
  , MatchedSigs(..)
  , mkSigMap
  ) where

import qualified Data.Map.Strict as M
import           Data.Map.Append.Strict (AppendMap(..))

import           HieTypes
import           HieUtils

import           Name
import           MatchSigs.Fingerprint (SigFingerprint, sigFingerprint)
import           MatchSigs.Matching (MatchedSigs(..))
import           MatchSigs.Sig (sigsFromHie)
import           Utils

import           GHC.DynFlags (baseDynFlags)

type SigMap = AppendMap SigFingerprint MatchedSigs

-- | Collect all the function definitions in the 'HieAST' that have isomorphic
-- type signatures.
mkSigMap :: HieAST HieTypeFix -> SigMap
mkSigMap node =
  let renderedSigs = modNodeChildren nameSigRendered node
      sigReps = modNodeChildren sigsFromHie node
      mkMatch n s r = (sigFingerprint r, MatchedSigs [(r, s, [n])])
      sigMatches = M.elems $ M.intersectionWithKey mkMatch renderedSigs sigReps
   in AppendMap $ M.fromListWith (<>) sigMatches

-- | Produce a 'Map' from function 'Name's to their rendered type signatures
nameSigRendered :: HieAST HieTypeFix -> M.Map Name String
nameSigRendered node
  | nodeHasAnnotation "FunBind" "HsBindLR" node
  , identNode : _ <- nodeChildren node
  , Right name : _ <- M.keys . nodeIdentifiers $ nodeInfo identNode
  , let renderedTy = unwords
                   . map (renderHieType baseDynFlags)
                   . nodeType
                   $ nodeInfo node
  = M.singleton name renderedTy
  | otherwise = mempty

