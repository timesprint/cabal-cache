module HaskellWorks.Ci.Assist.Text
  ( maybeStripPrefix
  , findLastIndex
  ) where

import Data.Maybe
import Data.Text  (Text)

import qualified Data.Text as Text

maybeStripPrefix :: Text -> Text -> Text
maybeStripPrefix prefix text = fromMaybe text (Text.stripPrefix prefix text)

findLastIndex :: Char -> Text -> Maybe Int
findLastIndex c =
  fst . Text.foldl' (\(i, p) c' ->
          if c' == c
            then (Just p, p+1)
            else (i, p+1)) (Nothing, 0)
