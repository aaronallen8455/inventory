import           Data.Map.Append.Strict (AppendMap(..))
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Test.Tasty
import           Test.Tasty.HUnit

import           DynFlags
import           HieBin
import           NameCache

import           DefCounts.ProcessHie
import           GHC.DynFlags
import           HieFile hiding (getCounters)
import           MatchSigs.ProcessHie
import           UseCounts.ProcessHie

getResources :: IO (NameCache, DynFlags)
getResources = (,) <$> mkNameCache <*> baseDynFlags

main :: IO ()
main = defaultMain $
  withResource getResources mempty $ \ioResources ->
  testGroup "Unit Tests"
    [ testCase "Valid Signature Matching" $ do
        (nameCache, dynFlags) <- ioResources
        sigMatchTest "T1" nameCache dynFlags [2]
        sigMatchTest "T2" nameCache dynFlags [2]
        sigMatchTest "T3" nameCache dynFlags [2]
        sigMatchTest "T4" nameCache dynFlags [2]
        sigMatchTest "T5" nameCache dynFlags [2]
        sigMatchTest "T6" nameCache dynFlags [3]
        sigMatchTest "T7" nameCache dynFlags [2,1]
        sigMatchTest "T8" nameCache dynFlags [2]
        sigMatchTest "T9" nameCache dynFlags [3]
        sigMatchTest "T10" nameCache dynFlags [5]
        sigMatchTest "T11" nameCache dynFlags [1,1]
        sigMatchTest "T12" nameCache dynFlags [1,1]
        sigMatchTest "T13" nameCache dynFlags [2]
        sigMatchTest "T14" nameCache dynFlags [2,1]

    , testCase "Definition Counting" $ do
        (nameCache, dynFlags) <- ioResources
        defCountTest "T15" nameCache dynFlags
          . AppendMap $ M.fromList [(Class, (2, 1)), (TyClInst, (2, 1))]
    ]

sigMatchTest :: String -> NameCache -> DynFlags -> [Int] -> IO ()
sigMatchTest testName nc dynFlags sigGroupSizes = do
  (_, _, AppendMap sigMap, _) <- getCounters testName nc dynFlags

  assertEqual testName sigGroupSizes
    . concatMap (map (\(_, _, names) -> length names) . getMatchedSigs)
    $ M.elems sigMap

defCountTest :: FilePath -> NameCache -> DynFlags -> DefCounter -> IO ()
defCountTest testName nc dynFlags expectedDefCount = do
  (defCount, _, _, _) <- getCounters testName nc dynFlags
  assertEqual testName expectedDefCount defCount

getHiePath :: String -> FilePath
getHiePath testName = "test/hie/HieSource/" <> testName <> ".hie"

getCounters :: String -> NameCache -> DynFlags -> IO (DefCounter, UsageCounter, SigMap, Sum Int)
getCounters testName nc dynFlags = do
  (hieFile, _) <- readHieFile nc $ getHiePath testName
  pure . hieFileToCounters dynFlags $ hie_file_result hieFile
