{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Network.AWS.Headers
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Headers where

import Data.CaseInsensitive (FoldCase, CI)
import Data.Text            (Text)
import Data.Time
import Network.AWS.Time

default (Text)

hAuth :: Text -> (CI Text, Text)
hAuth = ("Authorization",)

hAccept :: Text -> (CI Text, Text)
hAccept = ("Accept-Encoding",)

hDate :: Text -> (CI Text, Text)
hDate = ("Date",)

hHost :: Text -> (CI Text, Text)
hHost = ("Host",)

hAMZDate :: UTCTime -> (CI ByteString, ByteString)
hAMZDate = ("X-Amz-Date",) . Text.encodeUtf8 . formatISO8601

hAMZAuth :: Text -> (CI ByteString, ByteString)
hAMZAuth = ("X-Amzn-Authorization",)

hAMZToken :: ByteString -> (CI ByteString, ByteString)
hAMZToken = ("X-Amz-Security-Token",)
