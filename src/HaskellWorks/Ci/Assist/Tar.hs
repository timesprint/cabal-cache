module HaskellWorks.Ci.Assist.Tar
( packWith
, unpackWith
)
where

import Codec.Archive.Tar       as Tar
import Codec.Archive.Tar.Entry as Tar

import Control.Exception     (Exception, catch, throwIO)
import Control.Lens          ((&), (<&>))
import Control.Monad         (forM_)
import Data.Either           (fromRight)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import System.Directory      (copyFile, createDirectoryIfMissing, doesDirectoryExist, setModificationTime)
import System.FilePath       ((</>))
import System.IO.Error       (isPermissionError)
import System.IO.Unsafe      (unsafeInterleaveIO)

import qualified Data.ByteString.Char8      as BS.Char8
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.List                  as List
import qualified System.FilePath            as FilePath.Native

metaTarPath :: TarPath
metaTarPath = either error id (toTarPath False "METADATA.INFO")

packWith :: (FilePath -> LBS.ByteString -> LBS.ByteString) -- ^ Transform file content
  -> FilePath     -- ^ Base directory
  -> [FilePath]   -- ^ Files and directories to pack, relative to the base dir
  -> IO [Entry]
packWith transform baseDir paths0 =
  preparePaths baseDir paths0 >>= packPaths transform baseDir

unpackWith :: Exception e
  => (FilePath -> LBS.ByteString -> LBS.ByteString)
  -> FilePath
  -> Entries e
  -> IO ()
unpackWith transform baseDir (Next metaEntry entries) =
  if entryTarPath metaEntry == metaTarPath
    then case entryContent metaEntry of
          NormalFile bs _ -> do
            let names = lines (LC8.unpack bs)
            entries' <- either throwIO pure (entriesToList entries)
            let pairs = zip names entries'
            forM_ pairs (uncurry (unpackEntry transform baseDir))
          ecnt            -> fail $ "Impossible happened! Metadata file is not actually a file: " <> show ecnt
    else fail "Corrupted archive, first entry is meant to be metadata"

packPaths :: (FilePath -> LBS.ByteString -> LBS.ByteString) -> FilePath -> [FilePath] -> IO [Entry]
packPaths transform baseDir paths =
  let metaEntry = unlines paths & LC8.pack & Tar.fileEntry metaTarPath
  in interleave $ pure metaEntry : map (packPath transform baseDir) (zip [0..] paths)

updateFileContent :: (LBS.ByteString -> LBS.ByteString) -> Entry -> Entry
updateFileContent transform entry =
  case entryContent entry of
    NormalFile bs size ->
      let bs' = transform bs
      in entry { entryContent = NormalFile bs' (LBS.length bs') }
    _ -> entry

entriesToList :: Entries err -> Either err [Entry]
entriesToList = Tar.foldEntries (\e r -> fmap (e:) r) (Right []) Left

---------------------------- Mainly copied from TAR ---------------------------

unpackEntry :: (FilePath -> LBS.ByteString -> LBS.ByteString)
  -> FilePath     -- ^ Base directory
  -> FilePath     -- ^ Target entity path
  -> Entry        -- ^ Entry to extract
  -> IO ()
unpackEntry f baseDir realPath entry =
  case entryContent entry of
    NormalFile bs _ -> extractFile realPath (f realPath bs) (entryTime entry)
    Directory       -> extractDir realPath (entryTime entry)
    other           -> fail $ "Unsupported content type: " ++ show other

  where
    extractFile path content mtime = do
      -- Note that tar archives do not make sure each directory is created
      -- before files they contain, indeed we may have to create several
      -- levels of directory.
      createDirectoryIfMissing True absDir
      LBS.writeFile absPath content
      setModTime absPath mtime
      where
        absDir  = baseDir </> FilePath.Native.takeDirectory path
        absPath = baseDir </> path

    extractDir path mtime = do
      createDirectoryIfMissing True absPath
      setModTime absPath mtime
      where
        absPath = baseDir </> path

setModTime :: FilePath -> EpochTime -> IO ()
setModTime path t =
    setModificationTime path (posixSecondsToUTCTime (fromIntegral t))
      `catch` \e ->
        if isPermissionError e then return () else throwIO e

packPath :: (FilePath -> LBS.ByteString -> LBS.ByteString) -> FilePath -> (Int, FilePath) -> IO Entry
packPath transform baseDir (i, path) = do
  let filepath = baseDir </> path
  let isDir    = FilePath.Native.hasTrailingPathSeparator filepath
  tarpath      <- either fail pure (toTarPath isDir (show i))
  if isDir then packDirectoryEntry filepath tarpath
           else packFileEntry filepath tarpath <&> (\e -> updateFileContent (transform path) e)

preparePaths :: FilePath -> [FilePath] -> IO [FilePath]
preparePaths baseDir paths =
  concat <$> interleave
    [ do isDir  <- doesDirectoryExist (baseDir </> path)
         if isDir
           then do entries <- Tar.getDirectoryContentsRecursive (baseDir </> path)
                   let entries' = map (path </>) entries
                       dir = FilePath.Native.addTrailingPathSeparator path
                   if null path then return entries'
                                else return (dir : entries')
           else return [path]
    | path <- paths ]

interleave :: [IO a] -> IO [a]
interleave = unsafeInterleaveIO . go
  where
    go []     = return []
    go (x:xs) = do
      x'  <- x
      xs' <- interleave xs
      return (x':xs')
