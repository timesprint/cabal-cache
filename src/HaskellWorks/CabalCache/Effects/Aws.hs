{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

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
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UnicodeSyntax       #-}

module HaskellWorks.CabalCache.Effects.Aws
  ( awsMkEnv
  , runEffAws
  ) where

import Control.Monad
import Network.AWS.Types (LogLevel, Region)
import Polysemy

import qualified Antiope.Env          as IO
import qualified Data.ByteString.Lazy as LBS

data Aws m a where
  AwsMkEnv :: Region -> (LogLevel -> LBS.ByteString -> m ()) -> Aws m IO.Env

makeSem ''Aws

runEffAws
    :: Member (Embed IO) r
    => (forall x. Sem r x -> IO x)
    -> Sem (Aws ': r) a
    -> Sem r a
runEffAws lower = interpretH $ \case
  AwsMkEnv region cb -> do
    cb' <- bindT (uncurry cb)
    is  <- getInitialStateT
    env <- embed $ IO.mkEnv region . curry $ \bs ->
      void $ lower .@ runEffAws $ cb' (bs <$ is)
    pure (env <$ is)
