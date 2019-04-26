{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.SyncToArchive
  ( cmdSyncToArchive
  ) where

import Antiope.Core                    (toText)
import Antiope.Env                     (LogLevel, mkEnv)
import App.Commands.Options.Parser     (optsSyncToArchive)
import App.Static                      (homeDirectory)
import Control.Lens                    hiding ((<.>))
import Control.Monad                   (unless, when)
import Control.Monad.Except
import Control.Monad.Trans.Resource    (runResourceT)
import Data.Generics.Product.Any       (the)
import Data.List                       (isSuffixOf, (\\))
import Data.Semigroup                  ((<>))
import HaskellWorks.Ci.Assist.Core     (PackageInfo (..), Presence (..), Tagged (..), getPackages, loadPlan, relativePaths, relativePaths2)
import HaskellWorks.Ci.Assist.Location ((<.>), (</>))
import HaskellWorks.Ci.Assist.Metadata (addMetadata, createMetadata, getMetadataTarGroup)
import HaskellWorks.Ci.Assist.Show
import HaskellWorks.Ci.Assist.Version  (archiveVersion)
import Options.Applicative             hiding (columns)
import System.Directory                (createDirectoryIfMissing, doesDirectoryExist)

import qualified App.Commands.Options.Types        as Z
import qualified Codec.Archive.Tar                 as F
import qualified Codec.Compression.GZip            as F
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.ByteString.Lazy.Char8        as LC8
import qualified Data.Text                         as T
import qualified HaskellWorks.Ci.Assist.GhcPkg     as GhcPkg
import qualified HaskellWorks.Ci.Assist.Hash       as H
import qualified HaskellWorks.Ci.Assist.IO.Console as CIO
import qualified HaskellWorks.Ci.Assist.IO.Error   as IO
import qualified HaskellWorks.Ci.Assist.IO.File    as IO
import qualified HaskellWorks.Ci.Assist.IO.Lazy    as IO
import qualified HaskellWorks.Ci.Assist.IO.Tar     as IO
import qualified HaskellWorks.Ci.Assist.Types      as Z
import qualified System.Directory                  as IO
import qualified System.IO                         as IO
import qualified System.IO.Temp                    as IO
import qualified UnliftIO.Async                    as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

runSyncToArchive :: Z.SyncToArchiveOptions -> IO ()
runSyncToArchive opts = do
  let storePath           = opts ^. the @"storePath"
  let archiveUri          = opts ^. the @"archiveUri"
  let threads             = opts ^. the @"threads"
  let versionedArchiveUri = archiveUri </> archiveVersion
  let storePathHash       = H.hashStorePath storePath
  let scopedArchiveUri    = versionedArchiveUri </> T.pack storePathHash

  CIO.putStrLn $ "Store path: "       <> toText storePath
  CIO.putStrLn $ "Store path hash: "  <> T.pack storePathHash
  CIO.putStrLn $ "Archive URI: "      <> toText archiveUri
  CIO.putStrLn $ "Archive version: "  <> archiveVersion
  CIO.putStrLn $ "Threads: "          <> tshow threads

  mbPlan <- loadPlan
  case mbPlan of
    Right planJson -> do
      let compilerId = planJson ^. the @"compilerId"
      envAws <- mkEnv (opts ^. the @"region") (\_ _ -> pure ())
      let archivePath       = versionedArchiveUri </> compilerId
      let scopedArchivePath = scopedArchiveUri </> compilerId
      IO.createLocalDirectoryIfMissing archivePath
      IO.createLocalDirectoryIfMissing scopedArchivePath
      let baseDir = opts ^. the @"storePath"
      CIO.putStrLn "Extracting package list"

      packages <- getPackages baseDir planJson

      let storeCompilerPath           = baseDir </> T.unpack compilerId
      let storeCompilerPackageDbPath  = storeCompilerPath </> "package.db"

      storeCompilerPackageDbPathExists <- doesDirectoryExist storeCompilerPackageDbPath

      unless storeCompilerPackageDbPathExists $
        GhcPkg.init storeCompilerPackageDbPath

      CIO.putStrLn $ "Syncing " <> tshow (length packages) <> " packages"

      IO.withSystemTempDirectory "cabal-cache" $ \tempPath -> do
        CIO.putStrLn $ "Temp path: " <> tshow tempPath

        CIO.putStrLn "Copying package.db directory for transformation"
        let workingStoreCompilerPath = tempPath </> T.unpack compilerId

        IO.pooledForConcurrentlyN_ (opts ^. the @"threads") packages $ \pInfo -> do
          let archiveFileBasename = packageDir pInfo <.> ".tar.gz"
          let archiveFile         = versionedArchiveUri </> T.pack archiveFileBasename
          let scopedArchiveFile   = versionedArchiveUri </> T.pack storePathHash </> T.pack archiveFileBasename
          let packageStorePath    = baseDir </> packageDir pInfo
          let packageMetaPath     = workingStoreCompilerPath </> "_METADATA" </> packageDir pInfo
          let packageSharePath    = packageStorePath </> "share"
          archiveFileExists <- runResourceT $ IO.resourceExists envAws scopedArchiveFile

          unless archiveFileExists $ do
            packageStorePathExists <- doesDirectoryExist packageStorePath

            when packageStorePathExists $ void $ runExceptT $ IO.exceptWarn "Warning" $ do
              let rp2 = relativePaths2 storePath tempPath pInfo
              CIO.putStrLn $ "Creating " <> toText scopedArchiveFile

              let tempArchiveFile = tempPath </> archiveFileBasename

              meta <- createMetadata tempPath pInfo
              addMetadata meta "store-path" (LC8.pack baseDir)
              metaGroup <- getMetadataTarGroup meta

              IO.createTar tempArchiveFile (metaGroup : rp2)

              liftIO (LBS.readFile tempArchiveFile >>= IO.writeResource envAws scopedArchiveFile)

              shareEntries <- (\\ ["doc"]) <$> IO.listMaybeDirectory packageSharePath

              when (null shareEntries) $ IO.linkOrCopyResource envAws scopedArchiveFile archiveFile

    Left errorMessage -> do
      CIO.hPutStrLn IO.stderr $ "ERROR: Unable to parse plan.json file: " <> T.pack errorMessage

  return ()

cmdSyncToArchive :: Mod CommandFields (IO ())
cmdSyncToArchive = command "sync-to-archive"  $ flip info idm $ runSyncToArchive <$> optsSyncToArchive
