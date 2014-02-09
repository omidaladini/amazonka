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

import           Data.ByteString    (ByteString)
import qualified Data.Text.Encoding as Text
import           Data.Time
import           Network.AWS.Time
import           Network.HTTP.Types (Header)

default (ByteString)

hAccept :: ByteString -> Header
hAccept = ("Accept-Encoding",)

hDate :: ByteString -> Header
hDate = ("Date",)

hHost :: ByteString -> Header
hHost = ("Host",)

hAuth :: ByteString -> Header
hAuth = ("Authorization",)

hAMZAuth :: ByteString -> Header
hAMZAuth = ("X-Amzn-Authorization",)

hAMZDate :: UTCTime -> Header
hAMZDate = ("X-Amz-Date",) . Text.encodeUtf8 . formatISO8601

hAMZToken :: ByteString -> Header
hAMZToken = ("X-Amz-Security-Token",)

hAMZTarget :: ByteString -> Header
hAMZTarget = ("X-Amz-Target",)
