{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# LANGUAGE TemplateHaskell #-}
module Effects.Store
where

import Antiope.Core (runResAws, runAws)
import Antiope.Env (HasEnv)
import Antiope.S3 (S3Uri(..), ObjectKey(..), BucketName(..))
import Control.Resource     (unpackResourceT, ReleaseToken, release)
import Data.ByteString.Lazy (ByteString)
import Network.URI          (URI)
import Polysemy
import Polysemy.Resource
import Polysemy.Error
import Control.Monad (void)
import Data.Function ((&))
import Data.Functor ((<&>))

import qualified Antiope.S3.Lazy as S3
import qualified Antiope.S3 as S3
import qualified Network.URI as URI
import qualified Data.Text as Text

data Store m a where
  ReadContent   :: URI -> Store m (ReleaseToken, Maybe ByteString)
  Exists        :: URI -> Store m Bool
  WriteContent  :: URI -> ByteString -> Store m ()
  Close         :: ReleaseToken -> Store m ()

makeSem ''Store

data StoreError
  = InvalidUri URI
  deriving (Show, Eq, Ord)

runStoreS3 :: (HasEnv env, Member (Embed IO) r, Member (Error StoreError) r)
  => env
  -> Sem (Store ': r) a
  -> Sem r a
runStoreS3 e = interpret $ \case
  ReadContent u ->
    toS3Uri u >>= (embed . unpackResourceT . runAws e . S3.downloadFromS3Uri)
  WriteContent u c ->
    toS3Uri u >>= (embed . void . runResAws e . flip S3.putContent' c)
  Exists u ->
    toS3Uri u >>= (embed . runResAws e . S3.fileExists)
  Close t               -> embed $ release t


toS3Uri :: Member (Error StoreError) r => URI -> Sem r S3Uri
toS3Uri uri =
  maybe (throw (InvalidUri uri)) pure (toS3Uri' uri)

toS3Uri' :: URI -> Maybe S3Uri
toS3Uri' uri =
  let
    bkt = uri & URI.uriAuthority <&> URI.uriRegName <&> Text.pack <&> BucketName
    key = uri & URI.uriPath & drop 1 & Text.pack & ObjectKey
  in S3Uri <$> bkt <*> pure key

withStoreContent :: Members '[Store, Resource] r
  => URI
  -> (Maybe ByteString -> Sem r a)
  -> Sem r a
withStoreContent uri f =
  bracket
    (readContent uri)
    (close . fst)
    (f . snd)
