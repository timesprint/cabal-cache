{-# LANGUAGE TemplateHaskell     #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module HaskellWorks.CabalCache.Effects.Console
  ( Console(..)
  , runEffConsole
  , putStr
  , putStrLn
  ) where

import Data.Text (Text)
import Polysemy
import Prelude   hiding (putStr, putStrLn)

import qualified Data.Text.IO as T

data Console m a where
  PutStr   :: Text -> Console m ()
  PutStrLn :: Text -> Console m ()

makeSem ''Console

runEffConsole :: Member (Embed IO) r
  => Sem (Console ': r) a
  -> Sem r a
runEffConsole = interpret $ \case
  PutStr    s -> embed $ T.putStr   s
  PutStrLn  s -> embed $ T.putStrLn s
