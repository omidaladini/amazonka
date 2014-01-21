{-# LANGUAGE ViewPatterns #-}

-- Module      : Data.ByteString.Helpers
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Data.ByteString.Helpers where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text

strip :: Char -> ByteString -> ByteString
strip c = Text.encodeUtf8 . Text.dropAround (== c) . Text.decodeUtf8

stripPrefix :: ByteString -> ByteString -> ByteString
stripPrefix (Text.decodeUtf8 -> x) (Text.decodeUtf8 -> y) =
    Text.encodeUtf8 . fromMaybe y $ Text.stripPrefix x y

wrap :: Char -> ByteString -> ByteString
wrap c bs = case c `match` bs of
    (True,  True)  -> bs
    (False, True)  -> c `BS.cons` bs
    (True,  False) -> bs `BS.snoc` c
    (False, False) -> let b = BS.singleton c
                      in  BS.concat [b, bs, b]
  where
    match x xs
        | BS.null xs = (False, False)
        | otherwise  = (x == BS.head xs, x == BS.last xs)

