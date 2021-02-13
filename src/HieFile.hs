module HieFile
  ( getCounters
  , hieFileToCounters
  , mkNameCache
  ) where

import           Control.Exception (onException)
import           Control.Monad.State
import           Control.Parallel.Strategies (parList, rpar, using)
import           Data.Bifunctor
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe
import           Data.Monoid
import           System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, doesPathExist, listDirectory, withCurrentDirectory)
import           System.Environment (lookupEnv)
import           System.FilePath (isExtensionOf)

import           DynFlags (DynFlags)
import           HieBin
import           HieTypes
import           HieUtils
import           NameCache
import           UniqSupply (mkSplitUniqSupply)

import           DefCounts.ProcessHie
import           MatchSigs.ProcessHie
import           UseCounts.ProcessHie
import           Utils

getCounters :: DynFlags -> IO (DefCounter, UsageCounter, SigMap, Sum Int)
getCounters dynFlags = do
  files <- getHieFiles

  let counters = map (foldMap (hieFileToCounters dynFlags))
                     (chunks 50 files)
                   `using` parList rpar

  pure $ mconcat counters

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = let (x, xs') = splitAt n xs
               in x : chunks n xs'

hieFileToCounters :: DynFlags
                  -> HieFile
                  -> (DefCounter, UsageCounter, SigMap, Sum Int)
hieFileToCounters dynFlags hieFile =
  let hies = hie_asts hieFile
      asts = getAsts hies
      types = hie_types hieFile
      fullHies = flip recoverFullType types <$> hies

   in ( foldMap (modNodeChildren declLines) asts
      , foldMap (modNodeChildren usageCounter) asts
      , foldMap (mkSigMap dynFlags) $ getAsts fullHies
      , Sum . length . BS.lines $ hie_hs_src hieFile
      )

getHieFiles :: IO [HieFile]
getHieFiles = do
  hieDir <- fromMaybe ".hie" <$> lookupEnv "HIE_DIR"
  let notPathsFile = (/= "Paths_") . take 6
  filePaths <- filter notPathsFile <$> getHieFilesIn hieDir
    `onException` error "HIE file directory does not exist"
  nameCache <- mkNameCache
  evalStateT (traverse getHieFile filePaths) nameCache

getHieFile :: FilePath -> StateT NameCache IO HieFile
getHieFile filePath = StateT $ \nameCache ->
  first hie_file_result <$> readHieFile nameCache filePath

mkNameCache :: IO NameCache
mkNameCache = do
  uniqueSupply <- mkSplitUniqSupply 'z'
  pure $ initNameCache uniqueSupply []

-- | Recursively search for .hie files in given directory
getHieFilesIn :: FilePath -> IO [FilePath]
getHieFilesIn path | take 6 path == "Paths_" = pure []
getHieFilesIn path = do
  exists <-
    doesPathExist path

  if exists
    then do
      isFile <- doesFileExist path
      if isFile && "hie" `isExtensionOf` path
        then do
          path' <- canonicalizePath path
          return [path']
        else do
          isDir <-
            doesDirectoryExist path
          if isDir
            then do
              cnts <-
                listDirectory path
              withCurrentDirectory path (foldMap getHieFilesIn cnts)
            else
              return []
    else
      return []

