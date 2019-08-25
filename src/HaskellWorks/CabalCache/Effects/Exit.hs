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
  , runEffExit
  , exitWith
  , exitAndPrintOnFail
  ) where

import Polysemy

import qualified Data.Text                               as T
import qualified HaskellWorks.CabalCache.Effects.Console as E
import qualified Polysemy.Fail                           as E
import qualified System.Exit                             as IO

data Exit m a where
  ExitWith   :: IO.ExitCode -> Exit m ()

makeSem ''Exit

runEffExit :: Member (Embed IO) r
  => Sem (Exit ': r) a
  -> Sem r a
runEffExit = interpret $ \case
  ExitWith    s -> embed $ IO.exitWith   s

exitAndPrintOnFail :: Members
  '[Embed IO
  , E.Console
  ] r
  => Sem (E.Fail ': r) a -> Sem r a
exitAndPrintOnFail f = do
  result <- E.runFail f
  case result of
    Right a -> return a
    Left msg -> do
      E.errPutStrLn (T.pack msg)
      runEffExit (exitWith (IO.ExitFailure 1))
      error msg
