module Utils
  ( nodeHasAnnotation
  , foldNodeChildren
  ) where

import qualified Data.Set as S
import           Data.String

import           HieTypes

nodeHasAnnotation :: String -> String -> HieAST a -> Bool
nodeHasAnnotation constructor ty =
    S.member (fromString constructor, fromString ty)
  . nodeAnnotations
  . nodeInfo

foldNodeChildren :: Monoid m => (HieAST a -> m) -> HieAST a -> m
foldNodeChildren f = foldMap f . nodeChildren
