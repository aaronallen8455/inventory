import           Data.Bifunctor
import           Data.Map.Append.Strict (AppendMap(..))
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Test.Tasty
import           Test.Tasty.HUnit

import           DefCounts.ProcessHie
import           GHC.Api
import           GHC.DynFlags
import           GHC.Output hiding ((<>))
import           HieFile hiding (getCounters)
import           MatchSigs.ProcessHie
import           UseCounts.ProcessHie

main :: IO ()
main = defaultMain $
  withResource baseDynFlags mempty $ \ioResources ->
  testGroup "Unit Tests"
    [ testCase "Valid Signature Matching" $ do
        dynFlags <- ioResources
        sigMatchTest "T1" dynFlags [2]
        sigMatchTest "T2" dynFlags [2]
        sigMatchTest "T3" dynFlags [2]
        sigMatchTest "T4" dynFlags [2]
        sigMatchTest "T5" dynFlags [2]
        sigMatchTest "T6" dynFlags [3]
        sigMatchTest "T7" dynFlags [2,1]
        sigMatchTest "T8" dynFlags [2,2]
        sigMatchTest "T9" dynFlags [3]
        sigMatchTest "T10" dynFlags [5]
        sigMatchTest "T11" dynFlags [1,1]
        sigMatchTest "T12" dynFlags [1,1]
        sigMatchTest "T13" dynFlags [2]
        sigMatchTest "T14" dynFlags [1,2]
        sigMatchTest "T18" dynFlags [4]
        sigMatchTest "T19" dynFlags [3]
        sigMatchTest "T20" dynFlags [2,1,1]
        sigMatchTest "T21" dynFlags [1,1,1,1]

    , testCase "Definition Counting" $ do
        dynFlags <- ioResources
        defCountTest "T15" dynFlags
          . AppendMap $ M.fromList [(Class, (2, 1)), (ClassInst, (2, 1))]
        defCountTest "T17" dynFlags
          . AppendMap $ M.fromList
              [ (Class, (1, 1))
              , (Data, (1, 1))
              , (Newtype, (2, 1))
              , (Fam, (2, 2))
              , (Func, (2, 1))
              , (PatSyn, (1, 1))
              , (Syn, (1, 1))
              , (ClassInst, (2, 2))
              , (TyFamInst, (2, 2))
              , (ModImport, (1, 1))
              , (ExportThing, (5, 5))
              ]

    , testCase "Use Counts" $ do
        dynFlags <- ioResources
        useCountTest "T16" dynFlags
          [ ("foo", 1)
          , ("f", 1)
          , ("T", 1)
          , ("T", 3)
          ]
    ]

sigMatchTest :: String -> DynFlags -> [Int] -> IO ()
sigMatchTest testName dynFlags sigGroupSizes = do
  (_, _, AppendMap sigMap, _) <- getCounters testName dynFlags

  assertEqual testName sigGroupSizes
    . concatMap (map (\(_, _, names) -> length names) . getMatchedSigs)
    $ M.elems sigMap

defCountTest :: FilePath -> DynFlags -> DefCounter -> IO ()
defCountTest testName dynFlags expectedDefCount = do
  (defCount, _, _, _) <- getCounters testName dynFlags
  assertEqual testName expectedDefCount defCount

useCountTest :: FilePath -> DynFlags -> [(String, Int)] -> IO ()
useCountTest testName dynFlags expectedUseCount = do
  (_, AppendMap useCount, _, _) <- getCounters testName dynFlags
  let printName = showSDoc dynFlags . pprNameUnqualified
      actualCount = map (bimap printName usages) . M.toList
                  $ M.filter locallyDefined useCount
  assertEqual testName expectedUseCount actualCount

getHiePath :: String -> FilePath
getHiePath testName = "test/hie/HieSource/" <> testName <> ".hie"

getCounters :: String -> DynFlags -> IO (DefCounter, UsageCounter, SigMap, Sum Int)
getCounters testName dynFlags = do
  [hieFile] <- hieFilesFromPaths . (:[]) $ getHiePath testName
  pure $ hieFileToCounters dynFlags hieFile
