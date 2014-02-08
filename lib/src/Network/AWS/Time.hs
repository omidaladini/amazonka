-- Module      : Network.AWS.Time
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Time
    (
    -- * Formatters
      formatRFC822
    , formatISO8601
    , formatAWS
    , formatBasic

    -- * Parsers
    , parseISO8601
    ) where

import           Control.Error (note)
import           Data.Text     (Text)
import qualified Data.Text     as Text
import           Data.Time     (UTCTime)
import qualified Data.Time     as Time
import           System.Locale

formatRFC822, formatISO8601, formatAWS, formatBasic :: UTCTime -> Text
formatRFC822  = formatTime "%a, %d %b %Y %H:%M:%S GMT"
formatISO8601 = formatTime (iso8601DateFormat $ Just "%XZ")
formatAWS     = formatTime "%Y%m%dT%H%M%SZ"
formatBasic   = formatTime "%Y%m%d"

parseISO8601 :: String -> Either String UTCTime
parseISO8601 = note "unable to parse ISO8601 time"
    . Time.parseTime defaultTimeLocale "%FT%T%QZ"

formatTime :: String -> UTCTime -> Text
formatTime fmt = Text.pack . Time.formatTime defaultTimeLocale fmt
