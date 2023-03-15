{-# LANGUAGE CPP #-}
module HieFile
  ( Counters
  , getCounters
  , hieFileToCounters
  , hieFilesFromPaths
  , mkNameCache
  ) where

import           Control.Exception (onException)
import           Control.Monad
import qualified Data.Array as A
#if __GLASGOW_HASKELL__ < 900
import           Control.Monad.State
import           Data.Bifunctor
#endif
import qualified Data.ByteString.Char8 as BS
#if MIN_VERSION_ghc(9,0,0) && !MIN_VERSION_ghc(9,4,0)
import           Data.IORef
#endif
import           Data.Maybe
import           Data.Monoid
import           System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, doesPathExist, listDirectory, withCurrentDirectory)
import           System.Environment (lookupEnv)
import           System.Exit (exitFailure)
import           System.FilePath (isExtensionOf)

import           DefCounts.ProcessHie
import           GHC.Api hiding (hieDir)
import           MatchSigs.ProcessHie
import           UseCounts.ProcessHie
import           Utils

type Counters = ( DefCounter
                , UsageCounter
                , SigMap
                , Sum Int -- total num lines
                )

getCounters :: DynFlags -> IO Counters
getCounters dynFlags =
  foldMap (hieFileToCounters dynFlags) <$> getHieFiles

hieFileToCounters :: DynFlags
                  -> HieFile
                  -> Counters
hieFileToCounters dynFlags hieFile =
  let hies = hie_asts hieFile
      asts = getAsts hies
      types = hie_types hieFile
      fullHies = flip recoverFullType types <$> hies
      sourceLines = BS.lines $ hie_hs_src hieFile
      numLines = length sourceLines
      source = A.listArray (0, numLines - 1) sourceLines

   in ( foldMap (foldNodeChildren (declLines source)) asts
      , foldMap (foldNodeChildren usageCounter) asts
      , foldMap (mkSigMap dynFlags) $ getAsts fullHies
      , Sum numLines
      )

getHieFiles :: IO [HieFile]
getHieFiles = do
  hieDir <- fromMaybe ".hie" <$> lookupEnv "HIE_DIR"
  filePaths <- getHieFilesIn hieDir
    `onException` do
      putStrLn ("HIE file directory does not exist: " <> show hieDir)
      exitFailure
  when (null filePaths) $ do
    putStrLn $ "No HIE files found in dir: " <> show hieDir
    exitFailure
  hieFiles <- hieFilesFromPaths filePaths
  let srcFileExists = doesPathExist . hie_hs_file
  filterM srcFileExists hieFiles

#if MIN_VERSION_ghc(9,4,0)

hieFilesFromPaths :: [FilePath] -> IO [HieFile]
hieFilesFromPaths filePaths = do
  nameCache <- mkNameCache
  traverse (getHieFile nameCache) filePaths

getHieFile :: NameCache -> FilePath -> IO HieFile
getHieFile nameCache filePath =
  handleHieVersionMismatch filePath . fmap hie_file_result
    =<< readHieFileWithVersion
          (\(v, _) -> v == hieVersion)
          nameCache
          filePath

#elif __GLASGOW_HASKELL__ >= 900

hieFilesFromPaths :: [FilePath] -> IO [HieFile]
hieFilesFromPaths filePaths = do
  nameCacheRef <- newIORef =<< mkNameCache
  let updater = NCU $ atomicModifyIORef' nameCacheRef
  traverse (getHieFile updater) filePaths

getHieFile :: NameCacheUpdater -> FilePath -> IO HieFile
getHieFile ncUpdater filePath =
  handleHieVersionMismatch filePath . fmap hie_file_result
    =<< readHieFileWithVersion
          (\(v, _) -> v == hieVersion)
          ncUpdater
          filePath

#else

hieFilesFromPaths :: [FilePath] -> IO [HieFile]
hieFilesFromPaths filePaths = do
  nameCache <- mkNameCache
  evalStateT (traverse getHieFile filePaths) nameCache

getHieFile :: FilePath -> StateT NameCache IO HieFile
getHieFile filePath = StateT $ \nameCache ->
  handleHieVersionMismatch filePath . fmap (first hie_file_result)
    =<< readHieFileWithVersion
          (\(v, _) -> v == hieVersion)
          nameCache
          filePath

#endif

handleHieVersionMismatch :: FilePath -> Either HieHeader a -> IO a
handleHieVersionMismatch path = either errMsg pure where
  errMsg (ver, _ghcVer) = do
    putStrLn $ unlines
      [ "Incompatible hie file: " <> path
      , "hie files must be generated with the same GHC version used to compile inventory"
      , "Inventory was compiled with GHC version " <> show hieVersion
      , "The hie files for this project were generated with version " <> show ver
      ]
    exitFailure

mkNameCache :: IO NameCache
mkNameCache = do
#if MIN_VERSION_ghc(9,4,0)
  initNameCache 'z' []
#else
  uniqueSupply <- mkSplitUniqSupply 'z'
  pure $ initNameCache uniqueSupply []
#endif

-- | Recursively search for .hie files in given directory
getHieFilesIn :: FilePath -> IO [FilePath]
-- ignore Paths_* files generated by cabal
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

