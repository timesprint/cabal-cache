{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.PolyVersion
  ( cmdPolyVersion
  ) where

import App.Commands.Options.Parser (optsVersion)
import Data.List
import Data.Semigroup              ((<>))
import Options.Applicative         hiding (columns)

import qualified App.Commands.Options.Types         as Z
import qualified Data.Text                          as T
import qualified Data.Version                       as V
import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified Paths_cabal_cache                  as P

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

runPolyVersion :: Z.VersionOptions -> IO ()
runPolyVersion _ = do
  let V.Version {..} = P.version

  let version = intercalate "." $ fmap show versionBranch

  CIO.putStrLn $ "cabal-cache " <> T.pack version

cmdPolyVersion :: Mod CommandFields (IO ())
cmdPolyVersion = command "poly-version"  $ flip info idm $ runPolyVersion <$> optsVersion
