{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# LANGUAGE TemplateHaskell #-}
module Effects.FileSystem
where

import Data.ByteString.Lazy (ByteString)
import Polysemy

data FileSystem m a where
  WriteFile :: FilePath -> ByteString -> FileSystem m ()

makeSem ''FileSystem
