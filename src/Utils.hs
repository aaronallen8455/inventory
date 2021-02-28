{-# LANGUAGE CPP #-}
module Utils
  ( nodeHasAnnotation
  , getNodeInfo
  , foldNodeChildren
  ) where

#if __GLASGOW_HASKELL__ >= 900
import qualified Data.Map as M
#endif
import qualified Data.Set as S
import           Data.String

import           GHC.Api

#if __GLASGOW_HASKELL__ >= 900
mergeNodeInfo :: NodeInfo a -> NodeInfo a -> NodeInfo a
mergeNodeInfo (NodeInfo as ai ad) (NodeInfo bs bi bd) =
  NodeInfo (as <> bs) (ai <> bi) (M.unionWith (<>) ad bd)
#endif

-- | Extract node info for an AST. GHC 9 includes generated things that need to
-- be removed.
getNodeInfo :: HieAST a -> NodeInfo a
#if __GLASGOW_HASKELL__ >= 900
getNodeInfo = M.foldl' mergeNodeInfo emptyNodeInfo
            . M.delete GeneratedInfo -- removed ghc generated nodes
            . getSourcedNodeInfo . sourcedNodeInfo
#else
getNodeInfo = nodeInfo
#endif

nodeHasAnnotation :: String -> String -> HieAST a -> Bool
nodeHasAnnotation constructor ty =
    S.member (fromString constructor, fromString ty)
  . nodeAnnotations
  . getNodeInfo

foldNodeChildren :: Monoid m => (HieAST a -> m) -> HieAST a -> m
foldNodeChildren f = foldMap f . nodeChildren
