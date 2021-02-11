{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module MatchSigs.ProcessHie
  ( SigMap
  , MatchedSigs(..)
  , mkSigMap
  ) where

import           Control.Monad.State
import           Data.Either
import qualified Data.IntMap.Strict as IM
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Map.Append.Strict (AppendMap(..))
import qualified Data.Set as S

import           HieTypes

import           Name
import           FastString
import           Utils

type FreeVarIdx = Int

type SigMap = AppendMap SigFingerprint MatchedSigs

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

-- TODO don't need this?
-- | Used to check if the result types of two functions are similar enough to
-- warrant deeper comparison
data TypeFingerprint varIx
  = TFFree !varIx
  | TFName !FastString !(Maybe Name)
  | TFApp !(TypeFingerprint varIx) ![TypeFingerprint varIx]
  | TFFun ![TypeFingerprint varIx]
  | TFTuple ![TypeFingerprint ()]
  | TFKindSig !(TypeFingerprint varIx) !(TypeFingerprint varIx)
    -- we have to ignore vars completely in case of tuples because there's no
    -- totally reliable way to sort the elements in the presence of free vars
  deriving (Eq, Ord, Functor)

instance Show varIx => Show (TypeFingerprint varIx) where
  show (TFFree i) = show i
  show (TFName _ _) = "name"
  show (TFApp fp fps) = "app " <> show fp <> unwords (show <$> fps)
  show (TFFun fps) = "fun " <> unwords (show <$> fps)
  show (TFTuple fps) = "tuple " <> unwords (show <$> fps)
  show (TFKindSig ty ki) = "kind sig " <> show ty
                        <> " :: " <> show ki

-- | Using this as a Map key provides a heuristic for matching only the
-- signatures that share key characteristics rather than blindly comparing
-- every signature.
data SigFingerprint =
  SF { sfArgs     :: !Int -- the length of the [Sig]
     , sfFreeVars :: ![(Int, Int)] -- num occurances, num of vars that occur that many times
     , sfResult   :: !(TypeFingerprint FreeVarIdx)
     } deriving (Eq, Ord)

sigFreeVars :: [Sig FreeVarIdx] -> [(Int, Int)] -- Num occurrances <=> num of vars
sigFreeVars = M.toList
            . foldMap (`M.singleton` 1)
            . (foldMap . foldMap) (`M.singleton` 1) -- occurances of each var

resultFingerprint :: Sig FreeVarIdx -> TypeFingerprint FreeVarIdx
resultFingerprint s = evalState (go s) IM.empty
  where
    go :: Sig FreeVarIdx -> State (IM.IntMap FreeVarIdx) (TypeFingerprint FreeVarIdx)
    go = \case
      TyDescriptor str mbName -> pure $ TFName str mbName
      FreeVar i  ->
        do m <- get
           case m IM.!? i of
             Nothing -> let i' = IM.size m
                         in TFFree i' <$ modify' (IM.insert i i')
             Just i' -> pure $ TFFree i'
      Arg ss     -> TFFun <$> traverse go ss
      Qual _     -> pure $ TFFun [] -- ignore quals in result type
      Apply c as -> TFApp <$> (TFFun <$> traverse go c)
                          <*> traverse (fmap TFFun . traverse go) as
      VarCtx _   -> pure $ TFFun []
      Tuple xs   -> TFTuple . sort . map (() <$)
                <$> traverse (fmap TFFun . traverse go) xs
      KindSig ty ki -> TFKindSig <$> (TFFun <$> traverse go ty)
                                 <*> (TFFun <$> traverse go ki)

sigFingerprint :: [Sig FreeVarIdx] -> SigFingerprint
sigFingerprint sig
  | r : _ <- reverse sig
  = SF { sfArgs = length sig
       , sfFreeVars = sigFreeVars sig
       , sfResult = resultFingerprint r
       }
  | otherwise = error "empty Sig"

mkSigMap :: (HieTypeFix -> String) -> HieAST HieTypeFix -> SigMap
mkSigMap renderType node =
  let renderedSigs = modNodeChildren (nameSigRendered renderType) node
      sigReps = modNodeChildren nameSigRep node
      mkMatch n s r = (sigFingerprint r, MatchedSigs [(r, s, [n])])
      sigMatches = M.elems $ M.intersectionWithKey mkMatch renderedSigs sigReps
   in AppendMap $ M.fromListWith (<>) sigMatches

type SigMatches = ( [Sig FreeVarIdx]
                  , String -- rendered sig
                  , [Name] -- Names that share this signature
                  )

newtype MatchedSigs =
  MatchedSigs { getMatchedSigs :: [SigMatches] }

instance Semigroup MatchedSigs where
  MatchedSigs a <> MatchedSigs b
    = MatchedSigs
    . uncurry (++)
    -- fold compatible sigs from b in a, append the ones that are not compatible
    $ foldl' go (a, []) b
    where
      go (sigs, nonMatches) sig
        = let check (ss, False) s
                = case compatibleSigs s sig of
                    Just s' -> (s' : ss, True)
                    Nothing -> (s : ss, False)
              check (ss, True) s = (s : ss, True)
           in case foldl' check ([], False) sigs of
                (res, False) -> (res, sig : nonMatches)
                (res, True) -> (res, nonMatches)

instance Monoid MatchedSigs where
  mempty = MatchedSigs mempty

compatibleSigs :: SigMatches -> SigMatches -> Maybe SigMatches
compatibleSigs (sigA, str, namesA) (sigB, _, namesB)
  | matchArgs False mempty sigA sigB
  = Just (sigA, str, namesA ++ namesB)
  | otherwise = Nothing

matchArgs :: Bool -- True <=> result types have been checked
          -> IM.IntMap FreeVarIdx
          -> [Sig FreeVarIdx]
          -> [Sig FreeVarIdx]
          -> Bool
matchArgs _ _ [] [] = True
-- VarCtx and Qual are both expected to occur at the front of the list
matchArgs _ vm (VarCtx va : restA) (VarCtx vb : restB) =
  or $ do
    vm' <- varMatchings vm va vb
    pure $ matchArgs False vm' restA restB
matchArgs _ _ (VarCtx _ : _) _ = False
matchArgs _ _ _ (VarCtx _ : _) = False

matchArgs _ vm (Qual qa : restA) bs@(Qual _ : _) =
  let (qualsB, restB) = span isQual bs
   in or $ do
        -- try to match the quals in any order
        (i, Qual f : rest) <- zip (inits qualsB) (tails qualsB)
        guard $ matchArgs False vm qa f
        pure $ matchArgs False vm restA (i ++ rest ++ restB)
matchArgs _ _ (Qual _ : _) _ = False
matchArgs _ _ _ (Qual _ : _) = False

-- Extract the result types and make sure they match before going any further.
matchArgs False vm sa sb
  | ra : restA <- reverse sa
  , rb : restB <- reverse sb
  = matchArgs True vm [ra] [rb] && matchArgs True vm restA restB

matchArgs True vm (FreeVar ai : restA) (FreeVar bi : restB)
  | vm IM.!? bi == Just ai
  = matchArgs True vm restA restB

matchArgs True vm (TyDescriptor sa na : restA) (TyDescriptor sb nb : restB)
  | sa == sb
  , na == nb
  = matchArgs True vm restA restB

matchArgs True vm (Arg aa : restA) (Arg ab : restB)
  | matchArgs False vm aa ab
  = matchArgs True vm restA restB

matchArgs True vm (Apply ca aa : restA) (Apply cb ab : restB)
  | length aa == length ab
  , matchArgs False vm ca cb
  , and (zipWith (matchArgs False vm) aa ab)
  = matchArgs True vm restA restB

matchArgs True vm (Tuple [] : restA) (Tuple [] : restB)
  = matchArgs True vm restA restB
matchArgs True vm (Tuple (a : as) : restA) (Tuple bs : restB)
  | length as + 1 == length bs
  = or $ do
    (i, f : rest) <- zip (inits bs) (tails bs)
    -- order of elements in a tuple is not important
    guard $ matchArgs True vm a f
    guard $ matchArgs True vm [Tuple as] [Tuple $ i ++ rest]
    pure $ matchArgs True vm restA restB

matchArgs True vm (KindSig ta ka : restA) (KindSig tb kb : restB)
  = matchArgs False vm ta tb
 && matchArgs False vm ka kb
 && matchArgs True vm restA restB

matchArgs True vm (a : sa) sb
  = or $ do
      -- try at different positions of sb, argument order doesn't matter
      (i, f : rest) <- drop 1 $ zip (inits sb) (tails sb)
      guard $ matchArgs True vm [a] [f]
      pure $ matchArgs True vm sa (i ++ rest)

matchArgs _ _ _ _ = False

-- | generate all possible assignments of free variables from one sig to another
varMatchings :: IM.IntMap FreeVarIdx
             -> [FreeVarIdx]
             -> [FreeVarIdx]
             -> [IM.IntMap FreeVarIdx]
varMatchings existing xs ys
  | len /= length ys = [] -- should have same number of vars to be considered
  | otherwise = IM.union existing . IM.fromList . zip range
            <$> permutations ys
  where
    len = length xs
    mSize = IM.size existing
    range = [mSize .. mSize + len - 1]

-- TODO only include functions with arguments?
nameSigRep :: HieAST a -> M.Map Name [Sig FreeVarIdx]
nameSigRep node
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

nameSigRendered :: (HieTypeFix -> String) -> HieAST HieTypeFix -> M.Map Name String
nameSigRendered renderType node
  | nodeHasAnnotation "FunBind" "HsBindLR" node
  , identNode : _ <- nodeChildren node
  , Right name : _ <- M.keys . nodeIdentifiers $ nodeInfo identNode
  , let renderedTy = unwords
                   . map renderType
                   . nodeType
                   $ nodeInfo node
  = M.singleton name renderedTy
  | otherwise = mempty

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

