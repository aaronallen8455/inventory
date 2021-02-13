{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module MatchSigs.Sig
  ( FreeVarIdx
  , Sig(..)
  , sigsFromHie
  , isQual
  ) where

import           Control.Monad.State
import           Data.Either
import           Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import           HieTypes

import           Name
import           FastString
import           Utils

type FreeVarIdx = Int

sigsFromHie :: HieAST a -> M.Map Name [Sig FreeVarIdx]
sigsFromHie node
  | nodeHasAnnotation "TypeSig" "Sig" node
  , identNode : sigNode : _ <- nodeChildren node
  , Right name : _ <- M.keys . nodeIdentifiers $ nodeInfo identNode
  , let freeVars = extractFreeVars
  , let sig = evalState (mkSig sigNode) freeVars
        sig' | M.null freeVars = sig
             | otherwise = VarCtx (M.elems freeVars) : sig
        -- move qualifiers and var decls to front, collapsing var decls
        sig'' = frontLoadVarDecls $ frontLoadQuals sig'
  -- only include functions that take arguments
  , not $ null sig''
  = M.singleton name sig''

  | otherwise = mempty

  where
    extractFreeVars = M.fromList . (`zip` [0..])
                    . rights . M.keys
                    . nodeIdentifiers
                    $ nodeInfo node

-- | Traverses the 'HieAST', building the 'Sig' representation
mkSig :: HieAST a -> State (M.Map Name FreeVarIdx) [Sig FreeVarIdx]
mkSig node
  -- function ty
  | nodeHasAnnotation "HsFunTy" "HsType" node
  , arg : rest : _ <- nodeChildren node
  = do
    sigArg <- mkSig arg
    -- uncurry tuple arguments
    let sigArg' = case sigArg of
                    [Tuple xs] -> Arg <$> xs
                    a -> [Arg a]
    (sigArg' ++) <$> mkSig rest

  -- application
  | nodeHasAnnotation "HsAppTy" "HsType" node
  , con : rest <- nodeChildren node
  = fmap (:[]) $ Apply <$> mkSig con
                       <*> traverse mkSig rest

  -- constraint (qualifier)
  | nodeHasAnnotation "HsQualTy" "HsType" node
  , constraint : rest : _ <- nodeChildren node
  = do
    quals <- mkQuals constraint
    (quals ++) <$> mkSig rest

  -- parens
  | nodeHasAnnotation "HsParTy" "HsType" node
  , child : _ <- nodeChildren node
  = mkSig child

  -- free var decl
  | nodeHasAnnotation "HsForAllTy" "HsType" node
  , rest : userVarNodes <- reverse $ nodeChildren node
  = do
    vars <- foldM extractFreeVar [] userVarNodes
    (VarCtx vars :) <$> mkSig rest

  -- tuples
  | nodeHasAnnotation "HsTupleTy" "HsType" node
  , let children = nodeChildren node
  = fmap (:[]) $ Tuple <$> traverse mkSig children

  -- list ty
  | nodeHasAnnotation "HsListTy" "HsType" node
  , child : _ <- nodeChildren node
  = do
    c <- mkSig child
    pure [Apply [TyDescriptor "HsListTy" Nothing] [c]]

  -- kind sigs
  | nodeHasAnnotation "HsKindSig" "HsType" node
  , ty : ki : _ <- nodeChildren node
  = fmap (:[])
  $ KindSig <$> mkSig ty
            <*> mkSig ki

  -- any other type
  | (ty, "HsType") : _ <- S.toList . nodeAnnotations $ nodeInfo node
  , let mbName = extractName node
  = do
    freeVars <- get
    case mbName of
      Just name
        | Just idx <- freeVars M.!? name
        -> pure [FreeVar idx]
      _ -> pure [TyDescriptor ty mbName]

  | otherwise = pure []

  where
    extractName :: HieAST a -> Maybe Name
    extractName n
      | Right name : _ <- M.keys . nodeIdentifiers $ nodeInfo n
      = Just name
      | otherwise = Nothing

    extractFreeVar ixs n
      | nodeHasAnnotation "UserTyVar" "HsTyVarBndr" n
      , Just name <- extractName n
      = do
        ix <- gets M.size
        ix : ixs <$ modify' (M.insert name ix)
      | otherwise = pure ixs

    -- produce one ore more Quals from a constraint node
    mkQuals c
      | S.null . nodeAnnotations $ nodeInfo c
      = fmap Qual <$> traverse mkSig (nodeChildren c)
      | otherwise = fmap (:[]) $ Qual <$> mkSig c

-- TODO linear types
data Sig varIx
  = TyDescriptor !FastString !(Maybe Name)
  | FreeVar !varIx
  | Arg ![Sig varIx]
  | Qual ![Sig varIx]
  | Apply ![Sig varIx] ![[Sig varIx]]
  | VarCtx ![varIx]
  | Tuple ![[Sig varIx]]
  | KindSig ![Sig varIx] ![Sig varIx]
  deriving (Eq, Ord, Foldable, Functor)

isQual :: Sig a -> Bool
isQual (Qual _) = True
isQual _ = False

isVarDecl :: Sig a -> Bool
isVarDecl (VarCtx _) = True
isVarDecl _ = False

-- | Recursively transform the @['Sig' a]@s within a @'Sig' a@
recurseSig :: ([Sig a] -> [Sig a]) -> Sig a -> Sig a
recurseSig f (Arg s) = Arg . f $ recurseSig f <$> s
recurseSig f (Qual s) = Qual . f $ recurseSig f <$> s
recurseSig f (Apply a as) =
  Apply (f $ recurseSig f <$> a)
        (f . map (recurseSig f) <$> as)
recurseSig _ s = s

-- | Move qualifiers to the front of a sig, and recursively for sub-sigs
frontLoadQuals :: [Sig a] -> [Sig a]
frontLoadQuals = go . map (recurseSig frontLoadQuals) where
  go = uncurry (++) . partition isQual

-- | Move free var decls to the front of a sig, and recursively for sub-sigs
frontLoadVarDecls :: [Sig a] -> [Sig a]
frontLoadVarDecls = go . map (recurseSig frontLoadVarDecls)
  where
  go sig =
    let (varSigs, rest) = partition isVarDecl sig
     in collapseVarCtx varSigs : rest

  collapseVarCtx = VarCtx . concatMap getVars
  getVars (VarCtx vs) = vs
  getVars _ = []

