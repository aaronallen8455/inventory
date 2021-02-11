import           Data.Map.Append.Strict (AppendMap(..))
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Test.Tasty
import           Test.Tasty.HUnit

import           HieBin
import           NameCache

import           DefCounts.ProcessHie
import           HieFile hiding (getCounters)
import           MatchSigs.ProcessHie
import           UseCounts.ProcessHie

main :: IO ()
main = defaultMain $ do
  testCase "Valid Signature Matching" $ do
    nameCache <- mkNameCache

    sigMatchTest "T1" nameCache [2]
    sigMatchTest "T2" nameCache [2]
    sigMatchTest "T3" nameCache [2]
    sigMatchTest "T4" nameCache [2]
    sigMatchTest "T5" nameCache [2]
    sigMatchTest "T6" nameCache [3]
    sigMatchTest "T7" nameCache [2,1]
    sigMatchTest "T8" nameCache [2]
    sigMatchTest "T9" nameCache [3]
    sigMatchTest "T10" nameCache [5]
    sigMatchTest "T11" nameCache [1,1]
    sigMatchTest "T12" nameCache [1,1]
    sigMatchTest "T13" nameCache [2]
    sigMatchTest "T14" nameCache [2,1]

  testCase "Definition Counting" $ do
    defCountTest "T15" nameCache
      . AppendMap $ M.fromList [(Class, (2, 1)), (TyClInst, (2, 1))]

sigMatchTest :: String -> NameCache -> [Int] -> IO ()
sigMatchTest testName nc sigGroupSizes = do
  (_, _, AppendMap sigMap, _) <- getCounters testName nc

  assertEqual testName sigGroupSizes
    . concatMap (map (\(_, _, names) -> length names) . getMatchedSigs)
    $ M.elems sigMap

defCountTest :: FilePath -> NameCache -> DefCounter -> IO ()
defCountTest testName nc expectedDefCount = do
  (defCount, _, _, _) <- getCounters testName nc
  assertEqual testName expectedDefCount defCount

getHiePath :: String -> FilePath
getHiePath testName = "test/hie/HieSource/" <> testName <> ".hie"

getCounters :: String -> NameCache -> IO (DefCounter, UsageCounter, SigMap, Sum Int)
getCounters testName nc = do
  (hieFile, _) <- readHieFile nc $ getHiePath testName
  pure . hieFileToCounters (const "") $ hie_file_result hieFile
