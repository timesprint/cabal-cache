{-# LANGUAGE TemplateHaskell     #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module HaskellWorks.CabalCache.Effects.Log
  ( Log(..)
  , runLogConsole
  , logDebug
  , logInfo
  , logWarn
  , logError
  ) where

import Data.Text (Text)
import Polysemy

import qualified Data.Text.IO as T

data Log m a where
  LogDebug  :: Text -> Log m ()
  LogInfo   :: Text -> Log m ()
  LogWarn   :: Text -> Log m ()
  LogError  :: Text -> Log m ()

makeSem ''Log

runLogConsole :: Member (Embed IO) r
  => Sem (Log ': r) a
  -> Sem r a
runLogConsole = interpret $ \case
  LogDebug s -> embed $ T.putStrLn ("[Debug] " <> s)
  LogInfo  s -> embed $ T.putStrLn ("[Info] " <> s)
  LogWarn  s -> embed $ T.putStrLn ("[Warn] " <> s)
  LogError s -> embed $ T.putStrLn ("[Error] " <> s)
