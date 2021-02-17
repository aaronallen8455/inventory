module MatchSigs.Matching
  ( MatchedSigs(..)
  ) where

import           Data.List
import qualified Data.IntMap.Strict as IM

import           Name

import           MatchSigs.Sig

type SigMatches = ( [Sig FreeVarIdx] -- Sig shared by these 'Name's
                  , String -- rendered sig
                  , [Name] -- Names that share this signature
                  )

newtype MatchedSigs =
  MatchedSigs { getMatchedSigs :: [SigMatches] }

instance Semigroup MatchedSigs where
  (<>) = unionMatchedSigs

instance Monoid MatchedSigs where
  mempty = MatchedSigs mempty

-- | Create the union of two 'MatchedSigs' by checking if there a match in one
-- group for each sig in the other.
-- This is O(n^2) since there is no suitable ordering for sigs due to different
-- potential ordering of free vars.
unionMatchedSigs :: MatchedSigs -> MatchedSigs -> MatchedSigs
unionMatchedSigs (MatchedSigs a) (MatchedSigs b)
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

-- | Combines the names in two 'SigMatches' if the sigs match
compatibleSigs :: SigMatches -> SigMatches -> Maybe SigMatches
compatibleSigs (sigA, str, namesA) (sigB, _, namesB)
  | checkMatch mempty sigA sigB
  = Just (sigA, str, namesA ++ namesB)
  | otherwise = Nothing

-- | Check that two sigs are isomorphic
-- First step is to check that the contexts match.
checkMatch :: IM.IntMap FreeVarIdx
           -> [Sig FreeVarIdx]
           -> [Sig FreeVarIdx]
           -> Bool
-- VarCtx and Qual are both expected to occur at the front of the list
checkMatch vm (VarCtx va : restA) (VarCtx vb : restB) =
  or $ do
    vm' <- varMatchings vm va vb
    pure $ checkMatch vm' restA restB
checkMatch _ (VarCtx _ : _) _ = False
checkMatch _ _ (VarCtx _ : _) = False

-- Appearance order of quals not considered
checkMatch vm (Qual qa : restA) bs@(Qual _ : _)
  | let (qualsB, restB) = span isQual bs
        go (Qual f) = checkMatch vm qa f
        go _ = False
  , (i, _ : rest) <- break go qualsB
  = checkMatch vm restA (i ++ rest ++ restB)
checkMatch _ (Qual _ : _) _ = False
checkMatch _ _ (Qual _ : _) = False

checkMatch vm sa sb = checkResult vm sa sb

-- | Extract the result types and make sure they match before going any further.
checkResult :: IM.IntMap FreeVarIdx
            -> [Sig FreeVarIdx]
            -> [Sig FreeVarIdx]
            -> Bool
checkResult vm sa sb
  | ra : restA <- reverse sa
  , rb : restB <- reverse sb
  = checkArguments vm [ra] [rb] && checkArguments vm restA restB
checkResult _ _ _ = True

-- | After the result type has been removed, check the argument types.
checkArguments :: IM.IntMap FreeVarIdx
               -> [Sig FreeVarIdx]
               -> [Sig FreeVarIdx]
               -> Bool
checkArguments _ [] [] = True
checkArguments vm (FreeVar ai : restA) (FreeVar bi : restB)
  | vm IM.!? bi == Just ai
  = checkArguments vm restA restB

checkArguments vm (TyDescriptor sa na : restA) (TyDescriptor sb nb : restB)
  | sa == sb
  , na == nb
  = checkArguments vm restA restB

checkArguments vm (Arg aa : restA) (Arg ab : restB)
  | checkMatch vm aa ab
  = checkArguments vm restA restB

checkArguments vm (Apply ca aa : restA) (Apply cb ab : restB)
  | length aa == length ab
  , checkMatch vm ca cb
  , and (zipWith (checkMatch vm) aa ab)
  = checkArguments vm restA restB

checkArguments vm (Tuple [] : restA) (Tuple [] : restB)
  = checkArguments vm restA restB
checkArguments vm (Tuple (a : as) : restA) (Tuple bs : restB)
  | length as + 1 == length bs
  , (i, _ : rest) <- break (checkMatch vm a) bs
  , checkArguments vm [Tuple as] [Tuple $ i ++ rest]
  = checkArguments vm restA restB

checkArguments vm (KindSig ta ka : restA) (KindSig tb kb : restB)
  = checkMatch vm ta tb
 && checkMatch vm ka kb
 && checkArguments vm restA restB

checkArguments vm (a : sa) (b : sb)
  -- try at different positions of sb, argument order doesn't matter
  | (i, _ : rest) <- break (checkArguments vm [a] . pure) sb
  = checkArguments vm sa (b : i ++ rest)

checkArguments _ _ _ = False

-- | generate all possible assignments of free variables from one sig to another
varMatchings :: IM.IntMap FreeVarIdx
             -> [FreeVarIdx]
             -> [FreeVarIdx]
             -> [IM.IntMap FreeVarIdx]
varMatchings existing xs ys
  | length xs /= length ys = [] -- should have same number of vars to be considered
  | otherwise = IM.union existing . IM.fromList . zip xs
            <$> permutations ys

