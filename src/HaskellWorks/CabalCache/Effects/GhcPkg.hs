{-# LANGUAGE TemplateHaskell     #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UnicodeSyntax       #-}

module HaskellWorks.CabalCache.Effects.GhcPkg
  ( GhcPkg(..)
  , runEffGhcPkg
  , runGhcPkg
  ) where

import Data.Text                               (Text)
import HaskellWorks.CabalCache.Effects.Process
import Polysemy
import Prelude                                 hiding (putStr, putStrLn)

import qualified Data.Text.IO as T

data GhcPkg m a where
  RunGhcPkg   :: Text -> GhcPkg m ()

makeSem ''GhcPkg

runEffGhcPkg :: Members '[Embed IO, Process] r
  => Sem (GhcPkg ': r) a
  -> Sem r a
runEffGhcPkg = interpret $ \case
  RunGhcPkg    s -> do
    _ <- spawnProcess "ghc-pkg" []
    embed $ (pure () :: IO ())
