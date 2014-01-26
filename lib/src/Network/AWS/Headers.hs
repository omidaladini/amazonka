{-# LANGUAGE GADTs                      #-}
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

import           Control.Arrow
import           Data.ByteString      (ByteString)
import qualified Data.CaseInsensitive as CI
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text
import           Data.Time
import           Data.Time.Formatters
import           Network.HTTP.Types   (Header)

class ToHeader a where
    toHeader :: ByteString -> a -> Header

instance ToHeader ByteString where
    toHeader k = (CI.mk k,)

instance ToHeader Text where
    toHeader k = toHeader k . Text.encodeUtf8

data AnyHeader where
    (:::) :: ToHeader a => ByteString -> Maybe a -> AnyHeader

hAccept :: ByteString -> Header
hAccept = toHeader "Accept-Encoding"

hDate :: ByteString -> Header
hDate = toHeader "Date"

hHost :: ByteString -> Header
hHost = toHeader "Host"

hAuth :: ByteString -> Header
hAuth = toHeader "Authorization"

hAMZAuth :: ByteString -> Header
hAMZAuth = toHeader "X-Amzn-Authorization"

hAMZDate :: UTCTime -> Header
hAMZDate = toHeader "X-Amz-Date" . formatISO8601

hAMZToken :: ByteString -> Header
hAMZToken = toHeader "X-Amz-Security-Token"
