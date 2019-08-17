{-# LANGUAGE TemplateHaskell     #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module HaskellWorks.CabalCache.Effects.Exit
  ( Exit(..)
  , runExit
  , exitWith
  ) where

import Polysemy

import qualified System.Exit as IO

data Exit m a where
  ExitWith   :: IO.ExitCode -> Exit m ()

makeSem ''Exit

runExit :: Member (Embed IO) r
  => Sem (Exit ': r) a
  -> Sem r a
runExit = interpret $ \case
  ExitWith    s -> embed $ IO.exitWith   s
