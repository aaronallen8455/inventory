module MatchSigs.Matching
  ( MatchedSigs(..)
  ) where

import           Data.List
import qualified Data.IntMap.Strict as IM

import           Name

import           MatchSigs.Sig

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

-- | Combines the names in two 'SigMatches' if the sigs match
compatibleSigs :: SigMatches -> SigMatches -> Maybe SigMatches
compatibleSigs (sigA, str, namesA) (sigB, _, namesB)
  | checkMatch False mempty sigA sigB
  = Just (sigA, str, namesA ++ namesB)
  | otherwise = Nothing

-- | Check that two sigs are isomorphic
checkMatch :: Bool -- True <=> result types have been checked
          -> IM.IntMap FreeVarIdx
          -> [Sig FreeVarIdx]
          -> [Sig FreeVarIdx]
          -> Bool
checkMatch _ _ [] [] = True
-- VarCtx and Qual are both expected to occur at the front of the list
checkMatch _ vm (VarCtx va : restA) (VarCtx vb : restB) =
  or $ do
    vm' <- varMatchings vm va vb
    pure $ checkMatch False vm' restA restB
checkMatch _ _ (VarCtx _ : _) _ = False
checkMatch _ _ _ (VarCtx _ : _) = False

-- Appearance order of quals not considered
checkMatch _ vm (Qual qa : restA) bs@(Qual _ : _)
  | let (qualsB, restB) = span isQual bs
        go (Qual f) = checkMatch False vm qa f
        go _ = False
  , (i, _ : rest) <- break go qualsB
  = checkMatch False vm restA (i ++ rest ++ restB)
checkMatch _ _ (Qual _ : _) _ = False
checkMatch _ _ _ (Qual _ : _) = False

-- Extract the result types and make sure they match before going any further.
checkMatch False vm sa sb
  | ra : restA <- reverse sa
  , rb : restB <- reverse sb
  = checkMatch True vm [ra] [rb] && checkMatch True vm restA restB

checkMatch True vm (FreeVar ai : restA) (FreeVar bi : restB)
  | vm IM.!? bi == Just ai
  = checkMatch True vm restA restB

checkMatch True vm (TyDescriptor sa na : restA) (TyDescriptor sb nb : restB)
  | sa == sb
  , na == nb
  = checkMatch True vm restA restB

checkMatch True vm (Arg aa : restA) (Arg ab : restB)
  | checkMatch False vm aa ab
  = checkMatch True vm restA restB

checkMatch True vm (Apply ca aa : restA) (Apply cb ab : restB)
  | length aa == length ab
  , checkMatch False vm ca cb
  , and (zipWith (checkMatch False vm) aa ab)
  = checkMatch True vm restA restB

checkMatch True vm (Tuple [] : restA) (Tuple [] : restB)
  = checkMatch True vm restA restB
checkMatch True vm (Tuple (a : as) : restA) (Tuple bs : restB)
  | length as + 1 == length bs
  , (i, _ : rest) <- break (checkMatch True vm a) bs
  , checkMatch True vm [Tuple as] [Tuple $ i ++ rest]
  = checkMatch True vm restA restB

checkMatch True vm (KindSig ta ka : restA) (KindSig tb kb : restB)
  = checkMatch False vm ta tb
 && checkMatch False vm ka kb
 && checkMatch True vm restA restB

checkMatch True vm (a : sa) (b : sb)
  -- try at different positions of sb, argument order doesn't matter
  | (i, _ : rest) <- break (checkMatch True vm [a] . pure) sb
  = checkMatch True vm sa (b : i ++ rest)

checkMatch _ _ _ _ = False

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

