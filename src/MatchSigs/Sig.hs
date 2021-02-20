{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module MatchSigs.Sig
  ( FreeVarIdx
  , Sig(..)
  , sigsFromHie
  , sigFingerprint
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

-- TODO linear types
-- | The internal representation of a type. Function types are represented as a
-- linked list with the init elems being the context followed by arguments of
-- the function and the last being the result type.
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

instance Show varIx => Show (Sig varIx) where
  show (TyDescriptor fs _) = "TyDescriptor " <> show fs
  show (FreeVar ix) = "Var " <> show ix
  show (Arg a) = show a <> " -> "
  show (Qual q) = show q <> " => "
  show (Apply c args) = "App " <> show c <> " " <> show args
  show (VarCtx a) = "forall " <> show a <> ". "
  show (Tuple t) = "Tuple " <> show t
  show (KindSig x s) = show x <> " :: " <> show s

isQual :: Sig a -> Bool
isQual (Qual _) = True
isQual _ = False

isVarDecl :: Sig a -> Bool
isVarDecl (VarCtx _) = True
isVarDecl _ = False

-- | Produce a 'Map' from function 'Name's to their type signature's
-- internal representation.
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
  , not $ null sig''
  = M.singleton name sig''

  | otherwise = mempty

  where
    extractFreeVars = M.fromList . (`zip` [0..])
                    . rights . M.keys
                    . nodeIdentifiers
                    $ nodeInfo node

-- | Traverses the 'HieAST', building the representation for a function sig.
-- The `State` is for tracking free vars.
mkSig :: HieAST a -> State (M.Map Name FreeVarIdx) [Sig FreeVarIdx]
mkSig node
  -- function ty
  | nodeHasAnnotation "HsFunTy" "HsType" node
  , arg : rest : _ <- nodeChildren node
  = do
    sigArg <- mkSig arg
    -- curry tuple arguments
    let sigArg' = case sigArg of
                    [Tuple xs] | not (null xs) -> Arg <$> xs
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

-- | Recursively transform a '[Sig a]'.
recurseSig :: ([Sig a] -> [Sig a]) -> [Sig a] -> [Sig a]
recurseSig f = f . map go where
  go (Arg s) = Arg $ recurseSig f s
  go (Qual s) = Qual $ recurseSig f s
  go (Apply a as) =
    Apply (recurseSig f a)
          (recurseSig f <$> as)
  go (Tuple es) =
    Tuple (recurseSig f <$> es)
  go (KindSig ty ks) =
    KindSig (recurseSig f ty)
            (recurseSig f ks)
  go x@TyDescriptor{} = x
  go x@FreeVar{} = x
  go x@VarCtx{} = x

-- | Used to produce an orderable key for matching up signatures that are
-- likely to be equivalent. To allow for this, free vars must be homogenized
-- which is what 'void' does here.
sigFingerprint :: [Sig a] -> [Sig ()]
sigFingerprint = recurseSig go . map void
  where
    go = sort . map sortTuple
    sortTuple (Tuple es) = Tuple $ sort es
    sortTuple x = x

-- | Move qualifiers to the front of a sig, and recursively for sub-sigs
frontLoadQuals :: [Sig a] -> [Sig a]
frontLoadQuals = recurseSig go where
  go = uncurry (++) . partition isQual

-- | Move free var decls to the front of a sig, and recursively for sub-sigs
frontLoadVarDecls :: [Sig a] -> [Sig a]
frontLoadVarDecls = recurseSig go
  where
  go sig =
    let (varSigs, rest) = partition isVarDecl sig
     in collapseVarCtx varSigs : rest

  collapseVarCtx = VarCtx . concatMap getVars
  getVars (VarCtx vs) = vs
  getVars _ = []

