{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UnicodeSyntax       #-}

module HaskellWorks.CabalCache.Effects.Core
  ( mkCompilerContext
  ) where

import Data.Text      (Text)
import Polysemy
import Polysemy.Error

import qualified Control.Monad.Except          as CME
import qualified HaskellWorks.CabalCache.Core  as C
import qualified HaskellWorks.CabalCache.Types as Z
import qualified Polysemy.Error                as E

mkCompilerContext :: Members [Embed IO, Error Text] r
  => Z.PlanJson
  -> Sem r Z.CompilerContext
mkCompilerContext = E.fromEitherM . CME.runExceptT . C.mkCompilerContext @IO
