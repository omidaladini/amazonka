{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.GetSendStatistics
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the user's sending statistics. The result is a list of data points,
-- representing the last two weeks of sending activity. Each data point in the
-- list contains statistics for a 15-minute interval. This action is throttled
-- at one request per second. POST / HTTP/1.1 Date: Thu, 18 Aug 2011 22:23:01
-- GMT Host: email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=kwuk4eraA9HSfHySflgDKR6xK0JXjATIE7Uu5/FB4x4=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 99
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=GetSendStatistics
-- &Timestamp=2011-08-18T22%3A23%3A01.000Z 8 2011-08-03T19:23:00Z 0 0 0 7
-- 2011-08-03T06:53:00Z 0 0 0 . . . . c2b66ee5-c866-11e0-b17f-cddb0ab334db.
module Network.AWS.SES.GetSendStatistics where

import           Control.Monad
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import           Text.XML.Generic

import Network.AWS.SES.Service
import Network.AWS.SES.Types

data GetSendStatistics = GetSendStatistics
    deriving (Eq, Show, Generic)

instance ToQuery GetSendStatistics

instance AWSRequest GetSendStatistics where
    type Er GetSendStatistics = SESError
    type Rs GetSendStatistics = GetSendStatisticsResponse
    request = getQuery service "GetSendStatistics"

data GetSendStatisticsResponse = GetSendStatisticsResponse
    { gssrSendDataPoints :: [SendDataPoint]
      -- ^ A list of data points, each of which represents 15 minutes of activity.
    } deriving (Eq, Show, Generic)

instance FromXML GetSendStatisticsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetSendStatisticsResponse"
        :| ["GetSendStatisticsResult"]
