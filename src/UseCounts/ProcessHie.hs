{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module UseCounts.ProcessHie
  ( UsageCounter
  , UsageCount(..)
  , usageCounter
  ) where

import qualified Data.Map.Strict as M
import           Data.Map.Append.Strict (AppendMap(..))
import           Data.Maybe

import           HieTypes

import           Name
import           Utils

data UsageCount =
  UsageCount
    { usages :: !Int
    , locallyDefined :: !Bool
    } deriving Show

instance Semigroup UsageCount where
  UsageCount na da <> UsageCount nb db = UsageCount (na + nb) (da || db)

instance Monoid UsageCount where
  mempty = UsageCount 0 False

type UsageCounter = AppendMap Name UsageCount

usageCounter :: HieAST a -> UsageCounter
usageCounter node
  | nodeHasAnnotation "FunBind" "HsBindLR" node
  = foldMap findUsage (nodeChildren node)
 <> foldMap declaration (listToMaybe $ nodeChildren node)

  -- only get usages from instance declarations
  | any ((== "InstDecl") . snd) (nodeAnnotations $ nodeInfo node)
  = foldMap findUsage (nodeChildren node)

  | otherwise
  = foldMap declaration (nodeChildren node)
 <> foldMap findUsage (nodeChildren node)

-- | Accrues all the top-level declarations if all different types
declaration :: HieAST a -> UsageCounter
declaration node
  | any ((== "ConDecl") . snd) (nodeAnnotations $ nodeInfo node)
  = dataConDecl node
declaration node = M.foldMapWithKey f . nodeIdentifiers $ nodeInfo node
  where
    f (Right name) details = foldMap g (identInfo details) where
      declare = AppendMap $ M.singleton name (UsageCount 0 True)
      g (ValBind RegularBind ModuleScope _) = declare
      g (PatternBind ModuleScope _ _)       = declare
      g (Decl t _) | checkDeclType t        = declare
      g TyDecl                              = declare
      g ClassTyDecl{}                       = declare
      g _                                   = mempty
    f _ _ = mempty

    checkDeclType = \case
      InstDec -> False -- type fam instance is not a declaration
      _       -> True

-- | Handles data constructor declarations
dataConDecl :: HieAST a -> UsageCounter
dataConDecl node = foldMap declaration dec
                <> foldMap conField (nodeChildren =<< fields)
  where
    (dec, rest) = splitAt 1 $ nodeChildren node
    (fields, _) = splitAt 1 rest
    conField n
      | nodeHasAnnotation "ConDeclField" "ConDeclField" n
      = foldMap declaration (nodeChildren n)
      | otherwise = mempty

-- | Counts up the uses of all symbols in the AST.
findUsage :: HieAST a -> UsageCounter
findUsage node = (M.foldMapWithKey f . nodeIdentifiers . nodeInfo) node
              <> foldMap findUsage (nodeChildren node)
  where
    f (Right name) details = foldMap g (identInfo details) where
      use = AppendMap $ M.singleton name (UsageCount 1 False)
      g Use                                  = use
      g (ValBind InstanceBind ModuleScope _) = use
      g (Decl InstDec _)                     = use
      g (RecField RecFieldAssign _)          = use
      g _                                    = mempty
    f _ _ = mempty

