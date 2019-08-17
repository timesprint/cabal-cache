{-# LANGUAGE TemplateHaskell     #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module HaskellWorks.CabalCache.Effects.Process
  ( Process(..)
  , runProcess
  , spawnProcess
  , waitForProcess
  ) where

import Polysemy
import Prelude  hiding (putStr, putStrLn)

import qualified System.Exit    as IO
import qualified System.Process as IO

data Process m a where
  SpawnProcess   :: String -> [String] -> Process m IO.ProcessHandle
  WaitForProcess :: IO.ProcessHandle -> Process m IO.ExitCode

makeSem ''Process

runProcess :: Member (Embed IO) r
  => Sem (Process ': r) a
  -> Sem r a
runProcess = interpret $ \case
  SpawnProcess cmd args -> embed $ IO.spawnProcess cmd args
  WaitForProcess hProcess -> embed $ IO.waitForProcess hProcess
