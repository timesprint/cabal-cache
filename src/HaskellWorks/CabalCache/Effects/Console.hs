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
  , runConsole
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

runConsole :: Member (Embed IO) r
  => Sem (Console ': r) a
  -> Sem r a
runConsole = interpret $ \case
  PutStr    s -> embed $ T.putStr   s
  PutStrLn  s -> embed $ T.putStrLn s
