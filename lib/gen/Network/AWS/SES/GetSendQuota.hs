{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.GetSendQuota
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the user's current sending limits. This action is throttled at one
-- request per second. POST / HTTP/1.1 Date: Thu, 18 Aug 2011 22:22:36 GMT
-- Host: email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=W1YdiNOtf0jN3t7Lv63qhz7UZc3RrcmQpkGbopvnj/Y=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 94
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=GetSendQuota
-- &Timestamp=2011-08-18T22%3A22%3A36.000Z 127.0 200.0 1.0
-- 273021c6-c866-11e0-b926-699e21c3af9e.
module Network.AWS.SES.GetSendQuota where

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

-- | Convenience method utilising default fields where applicable.
getSendQuota :: AWS (Either SESError GetSendQuotaResponse)
getSendQuota = undefined GetSendQuota

data GetSendQuota = GetSendQuota
    deriving (Eq, Show, Generic)

instance ToQuery GetSendQuota

instance AWSRequest GetSendQuota where
    type Er GetSendQuota = SESError
    type Rs GetSendQuota = GetSendQuotaResponse
    request = getQuery service "GetSendQuota"

data GetSendQuotaResponse = GetSendQuotaResponse
    { gsqrMax24HourSend :: Maybe Double
      -- ^ The maximum number of emails the user is allowed to send in a 24-hour
      -- interval.
    , gsqrMaxSendRate :: Maybe Double
      -- ^ The maximum number of emails the user is allowed to send per second.
    , gsqrSentLast24Hours :: Maybe Double
      -- ^ The number of emails sent during the previous 24 hours.
    } deriving (Eq, Show, Generic)

instance FromXML GetSendQuotaResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetSendQuotaResponse"
        :| ["GetSendQuotaResult"]
