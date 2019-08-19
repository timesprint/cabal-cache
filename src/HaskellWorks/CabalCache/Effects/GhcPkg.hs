{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module HaskellWorks.CabalCache.Effects.GhcPkg
  ( runGhcPkg
  ) where

import Polysemy
import Polysemy.Fail
import Prelude       hiding (putStr, putStrLn)

import qualified HaskellWorks.CabalCache.Effects.Console as E
import qualified HaskellWorks.CabalCache.Effects.Process as E
import qualified System.Exit                             as IO

runGhcPkg :: Members
    [ E.Process
    , E.Console
    , Fail
    ] r
  => [String]
  -> Sem r ()
runGhcPkg args = do
  hGhcPkg <- E.spawnProcess "ghc-pkg" args
  exitCode <- E.waitForProcess hGhcPkg
  case exitCode of
    IO.ExitFailure _ -> fail "ERROR: Unable to recache package db"
    _                -> return ()
