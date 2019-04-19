{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Ci.Assist.PackageConfig where

import Data.ByteString.Char8       (pack)
import Data.ByteString.Lazy.Search (replace)
import HaskellWorks.Ci.Assist.Core
import HaskellWorks.Ci.Assist.Tar

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS

storePathMacro :: BS.ByteString
storePathMacro = "${STORE_PATH}"

-- templateConfig :: FilePath -> LBS.ByteString -> LBS.ByteString
-- templateConfig storePath = replace (pack storePath) storePathMacro
templateConfig :: PackageInfo
  -> FilePath       -- ^ base store path
  -> FilePath       -- ^ tar entry path
  -> LBS.ByteString -- ^ file content
  -> LBS.ByteString
templateConfig pkg storePath entryPath bs =
  case confPath pkg of
    Tagged conf Present | conf == entryPath -> replace (pack storePath) storePathMacro bs
    _                                       -> bs

unTemplateConfig :: PackageInfo
  -> FilePath       -- ^ base store path
  -> FilePath       -- ^ tar entry path
  -> LBS.ByteString -- ^ file content
  -> LBS.ByteString
unTemplateConfig pkg storePath entryPath bs =
  case confPath pkg of
    Tagged conf _ | conf == entryPath -> replace storePathMacro (pack storePath) bs
    _                                 -> bs
