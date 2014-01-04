{-# LANGUAGE OverloadedStrings #-}

-- Module      : Helpers
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Helpers where

import           Data.Char
import           Data.Maybe
import           Data.Monoid
import           Data.Text   (Text)
import qualified Data.Text   as Text

lowerWith :: Char -> String -> String
lowerWith x = map toLower
    . tail
    . dropWhile (not . (x ==))
    . concatMap f
  where
    f c | isUpper c = [x, toLower c]
        | otherwise = [c]

strip :: Text -> Text -> Text
strip delim = f Text.stripSuffix . f Text.stripPrefix
  where
    f g x = fromMaybe x $ g delim x

normalise :: Text -> [Text]
normalise = wrapLines 76
    . f
    . Text.strip
    . Text.unwords
    . map (stripTags . Text.strip)
    . Text.lines
  where
    f ""  = ""
    f " " = ""
    f t   = endWith "." t

wrapLines :: Int -> Text -> [Text]
wrapLines n = map (Text.pack . unwords) . go 0 [] . words . Text.unpack
    where
      go :: Int -> [String] -> [String] -> [[String]]
      go _ acc [] = [reverse acc]
      go k acc ws@(w : rest)
          | l >= n     = reverse acc : [w] : go 0 [] rest
          | k + l >= n = reverse acc       : go 0 [] ws
          | otherwise  = go (k + l + 1) (w : acc) rest
        where
          l = length w

endWith :: Text -> Text -> Text
endWith x y
    | x `Text.isSuffixOf` y = y
    | otherwise             = y <> x

stripTags :: Text -> Text
stripTags t
    | Text.null t = Text.empty
    | otherwise   =
        case Text.head t of
            '<' -> stripTags . Text.drop 1 . Text.dropWhile (/= '>') $ Text.tail t
            _   -> Text.cons (Text.head t) . stripTags $ Text.tail t

pascalize :: Text -> Text
pascalize = Text.concat . split . Text.toTitle

camelize :: Text -> Text
camelize = lowerFirst . pascalize

underscore :: Text -> Text
underscore = Text.intercalate (Text.singleton '_') . split

hyphenate :: Text -> Text
hyphenate = Text.intercalate (Text.singleton '-') . split

split :: Text -> [Text]
split = filter (/= "") . Text.split f
  where
    f ' '  = True
    f '\n' = True
    f '_'  = True
    f '-'  = True
    f  _   = False

lowerFirst :: Text -> Text
lowerFirst t
    | Text.null t = t
    | isUpper h   = toLower h `Text.cons` Text.tail t
    | otherwise   = t
  where
    h = Text.head t

upperFirst :: Text -> Text
upperFirst t
    | Text.null t = t
    | isLower h   = toUpper h `Text.cons` Text.tail t
    | otherwise   = t
  where
    h = Text.head t
