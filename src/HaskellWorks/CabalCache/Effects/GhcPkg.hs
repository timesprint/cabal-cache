{-# LANGUAGE TemplateHaskell     #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module HaskellWorks.CabalCache.Effects.GhcPkg
  ( GhcPkg(..)
  , runGhcPkg
  , putStr
  , putStrLn
  ) where

import Data.Text (Text)
import Polysemy
import Prelude   hiding (putStr, putStrLn)

import qualified Data.Text.IO as T

data GhcPkg m a where
  PutStr   :: Text -> GhcPkg m ()
  PutStrLn :: Text -> GhcPkg m ()

makeSem ''GhcPkg

runGhcPkg :: Member (Embed IO) r
  => Sem (GhcPkg ': r) a
  -> Sem r a
runGhcPkg = interpret $ \case
  PutStr    s -> embed $ T.putStr   s
  PutStrLn  s -> embed $ T.putStrLn s

-- runGhcPkg :: [String] -> IO ()
-- runGhcPkg params = do
--   hGhcPkg2 <- spawnProcess "ghc-pkg" params
--   exitCodeGhcPkg2 <- waitForProcess hGhcPkg2
--   case exitCodeGhcPkg2 of
--     ExitFailure _ -> do
--       IO.hPutStrLn IO.stderr "ERROR: Unable to recache package db"
--       exitWith (ExitFailure 1)
--     _ -> return ()
