import           Data.Bifunctor
import           Data.Map.Append.Strict (AppendMap(..))
import qualified Data.Map.Strict as M
import           Test.Tasty
import           Test.Tasty.HUnit

import           HieBin
import           NameCache

import           HieFile
import           MatchSigs.ProcessHie

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
    sigMatchTest "T7" nameCache [2, 1]
    sigMatchTest "T8" nameCache [2]

sigMatchTest :: FilePath -> NameCache -> [Int] -> IO ()
sigMatchTest fileName nc sigGroupSizes = do
  let hiePath = "test/hie/HieSource/" <> fileName <> ".hie"
  (hieFile, _) <- first hie_file_result <$> readHieFile nc hiePath

  let (_, _, AppendMap sigMap, _) = hieFileToCounters (const "") hieFile

  assertEqual fileName sigGroupSizes
    . concatMap (map (\(_, _, names) -> length names) . getMatchedSigs)
    $ M.elems sigMap
