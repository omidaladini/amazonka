{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DescribeEventSubscriptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists all the subscription descriptions for a customer account. The
-- description for a subscription includes SubscriptionName, SNSTopicARN,
-- CustomerID, SourceType, SourceID, CreationTime, and Status. If you specify
-- a SubscriptionName, lists the description for that subscription.
-- https://rds.us-east-1.amazonaws.com/ ?Action=DescribeEventSubscriptions
-- &MaxRecords=100 &Version=2013-01-10 &SignatureVersion=4
-- &SignatureMethod=HmacSHA256 &Timestamp=20130128T004543Z &AWSAccessKeyId=
-- &Signature= true 012345678901 active 2013-01-28 00:29:23.736
-- EventSubscription01 arn:aws:sns:us-east-1:012345678901:EventSubscription01
-- true 012345678901 active 2013-01-28 00:29:42.851 EventSubscription02
-- arn:aws:sns:us-east-1:012345678901:EventSubscription01
-- 0ce48079-68e4-11e2-91fe-5daa8e68c7d4.
module Network.AWS.RDS.DescribeEventSubscriptions where

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

import Network.AWS.RDS.Service
import Network.AWS.RDS.Types

-- | Convenience method utilising default fields where applicable.
describeEventSubscriptions :: AWS (Either RDSError DescribeEventSubscriptionsResponse)
describeEventSubscriptions = undefined $ DescribeEventSubscriptions
    { desnMarker = Nothing
    , desnMaxRecords = Nothing
    , desnSubscriptionName = Nothing
    }

data DescribeEventSubscriptions = DescribeEventSubscriptions
    { desnMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DescribeOrderableDBInstanceOptions request. If this parameter is specified,
      -- the response includes only records beyond the marker, up to the value
      -- specified by MaxRecords .
    , desnMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a pagination token called a
      -- marker is included in the response so that the remaining results can be
      -- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
    , desnSubscriptionName :: Maybe Text
      -- ^ The name of the RDS event notification subscription you want to describe.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeEventSubscriptions

instance AWSRequest DescribeEventSubscriptions where
    type Er DescribeEventSubscriptions = RDSError
    type Rs DescribeEventSubscriptions = DescribeEventSubscriptionsResponse
    request = getQuery service "DescribeEventSubscriptions"

instance AWSPager DescribeEventSubscriptions where
    next rq rs
        | Just x <- desnrsMarker rs = Just $ rq { desnMarker = Just x }
        | otherwise = Nothing

data DescribeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse
    { desnrsEventSubscriptionsList :: [EventSubscription]
      -- ^ A list of EventSubscriptions data types.
    , desnrsMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DescribeOrderableDBInstanceOptions request. If this parameter is specified,
      -- the response includes only records beyond the marker, up to the value
      -- specified by MaxRecords.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeEventSubscriptionsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeEventSubscriptionsResponse"
        :| ["DescribeEventSubscriptionsResult"]
