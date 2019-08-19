{-# LANGUAGE TemplateHaskell     #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module HaskellWorks.CabalCache.Effects.FileSystem
  ( FileSystem(..)
  , runEffFileSystem
  , readFile
  , writeFile
  ) where

import Polysemy
import Prelude  hiding (readFile, writeFile)

import qualified Data.ByteString.Lazy as LBS

data FileSystem m a where
  ReadFile   :: FilePath -> FileSystem m LBS.ByteString
  WriteFile  :: FilePath -> LBS.ByteString -> FileSystem m ()

makeSem ''FileSystem

runEffFileSystem :: Member (Embed IO) r
  => Sem (FileSystem ': r) a
  -> Sem r a
runEffFileSystem = interpret $ \case
  ReadFile fp -> embed $ LBS.readFile fp
  WriteFile fp contents -> embed $ LBS.writeFile fp contents
