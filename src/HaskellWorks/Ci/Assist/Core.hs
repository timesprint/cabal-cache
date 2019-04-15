{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
module HaskellWorks.Ci.Assist.Core
  ( PackageInfo(..)
  , Tagged(..)
  , Presence(..)
  , getPackages
  , relativePaths
  , loadPlan
  ) where

import Control.DeepSeq           (NFData)
import Control.Lens              hiding ((<.>))
import Control.Monad             (forM)
import Data.Aeson                (eitherDecode)
import Data.Bool                 (bool)
import Data.Generics.Product.Any (the)
import Data.Maybe                (maybeToList)
import Data.Semigroup            ((<>))
import Data.Text                 (Text)
import GHC.Generics              (Generic)
import System.FilePath           ((<.>), (</>))

import qualified Data.ByteString.Lazy         as LBS
import qualified Data.List                    as List
import qualified Data.Text                    as Text
import qualified HaskellWorks.Ci.Assist.Text  as Text
import qualified HaskellWorks.Ci.Assist.Types as Z
import qualified System.Directory             as IO

type CompilerId = Text
type PackageId  = Text
type PackageDir = FilePath
type ConfPath   = FilePath
type Library    = FilePath

data Presence   = Present | Absent deriving (Eq, Show, NFData, Generic)

data Tagged a t = Tagged
  { value :: a
  , tag   :: t
  } deriving (Eq, Show, Generic, NFData)

data PackageInfo = PackageInfo
  { compilerId     :: CompilerId
  , packageId      :: PackageId
  , shortPackageId :: PackageId
  , packageDir     :: PackageDir
  , confPath       :: Tagged ConfPath Presence
  , libs           :: [Library]
  } deriving (Show, Eq, Generic, NFData)

relativePaths :: PackageInfo -> [FilePath]
relativePaths pInfo = mempty
  <>  ([pInfo ^. the @"confPath"] & filter ((== Present) . (^. the @"tag")) <&> (^. the @"value"))
  <>  [packageDir pInfo]
  <>  (pInfo ^. the @"libs")

getPackages :: FilePath -> Z.PlanJson -> IO [PackageInfo]
getPackages basePath planJson = forM packages (mkPackageInfo basePath compilerId)
  where compilerId :: Text
        compilerId = planJson ^. the @"compilerId"
        packages :: [Z.Package]
        packages = planJson ^.. the @"installPlan" . each . filtered predicate
        predicate :: Z.Package -> Bool
        predicate package = package ^. the @"packageType" /= "pre-existing" && package ^. the @"style" == Just "global"

loadPlan :: IO (Either String Z.PlanJson)
loadPlan =
  eitherDecode <$> LBS.readFile ("dist-newstyle" </> "cache" </> "plan.json")

-------------------------------------------------------------------------------
mkPackageInfo :: FilePath -> CompilerId -> Z.Package -> IO PackageInfo
mkPackageInfo basePath cid pkg = do
  let pid               = pkg ^. the @"id"
  let compilerPath      = basePath </> Text.unpack cid
  let relativeConfPath  = Text.unpack cid </> "package.db" </> Text.unpack pid <.> ".conf"
  let absoluteConfPath  = basePath </> relativeConfPath
  let libPath           = compilerPath </> "lib"
  let relativeLibPath   = Text.unpack cid </> "lib"
  let libPrefix         = "libHS" <> pid
  absoluteConfPathExists <- IO.doesFileExist absoluteConfPath
  libPathExists <- IO.doesDirectoryExist libPath
  libFiles <- getLibFiles relativeLibPath libPath libPrefix
  return PackageInfo
    { compilerId      = cid
    , packageId       = pid
    , shortPackageId  = shortenId pid
    , packageDir      = Text.unpack cid </> Text.unpack pid
    , confPath        = Tagged relativeConfPath (bool Absent Present absoluteConfPathExists)
    , libs            = libFiles
    }

getLibFiles :: FilePath -> FilePath -> Text -> IO [Library]
getLibFiles relativeLibPath libPath libPrefix =
  fmap (relativeLibPath </>) . filter (List.isPrefixOf (Text.unpack libPrefix)) <$> IO.listDirectory libPath

shortenId :: PackageId -> PackageId
shortenId pid =
  case Text.findLastIndex '-' pid of
    Nothing  -> pid
    Just idx -> Text.take (idx + 5) pid
