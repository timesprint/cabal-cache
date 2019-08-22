{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module HaskellWorks.CabalCache.Effects.Stm
  ( asyncAwait
  ) where

import Control.Concurrent.STM (STM)
import Control.Monad
import Polysemy
import Polysemy.Async
import Prelude                hiding (putStr, putStrLn)

import qualified Control.Concurrent     as IO
import qualified Control.Concurrent.STM as STM

asyncAwait :: Members '[Embed IO, Async] r
  => Int -> Sem r a -> Sem r [Maybe a]
asyncAwait n f = do
  bs <- forM [1..n] . const $ async f
  rs <- forM bs await
  return rs
