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
  , awsLogger
  ) where

import Antiope.Env                  (LogLevel (..), Region)
import Control.Concurrent           (myThreadId)
import Control.Monad
import HaskellWorks.CabalCache.Show
import Polysemy

import qualified Antiope.Env                             as IO
import qualified Data.ByteString.Lazy                    as LBS
import qualified Data.ByteString.Lazy.Char8              as LC8
import qualified Data.Text.Encoding                      as T
import qualified HaskellWorks.CabalCache.Effects.Console as E

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

awsLogger
  :: Members '[E.Console, Embed IO] r
  => Maybe LogLevel -> LogLevel -> LC8.ByteString
  -> Sem r ()
awsLogger maybeConfigLogLevel msgLogLevel message =
  forM_ maybeConfigLogLevel $ \configLogLevel ->
    when (msgLogLevel <= configLogLevel) $ do
      threadId <- embed myThreadId
      E.errPutStrLn $ "[" <> tshow msgLogLevel <> "] [tid: " <> tshow threadId <> "]"  <> text
  where text = T.decodeUtf8 $ LBS.toStrict message
