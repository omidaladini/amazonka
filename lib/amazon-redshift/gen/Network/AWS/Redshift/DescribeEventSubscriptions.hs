{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DescribeEventSubscriptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists descriptions of all the Amazon Redshift event notifications
-- subscription for a customer account. If you specify a subscription name,
-- lists the description for that subscription.
module Network.AWS.Redshift.DescribeEventSubscriptions where

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

import Network.AWS.Redshift.Service
import Network.AWS.Redshift.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeEventSubscriptions :: DescribeEventSubscriptions
describeEventSubscriptions = DescribeEventSubscriptions
    { desnMarker = Nothing
    , desnMaxRecords = Nothing
    , desnSubscriptionName = Nothing
    }

data DescribeEventSubscriptions = DescribeEventSubscriptions
    { desnMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DescribeOrderableClusterOptions request. If this parameter is specified,
      -- the response includes only records beyond the marker, up to the value
      -- specified by MaxRecords.
    , desnMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a pagination token called a
      -- marker is included in the response so that the remaining results can be
      -- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
    , desnSubscriptionName :: Maybe Text
      -- ^ The name of the Amazon Redshift event notification subscription to be
      -- described.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeEventSubscriptions

instance AWSRequest DescribeEventSubscriptions where
    type Er DescribeEventSubscriptions = RedshiftError
    type Rs DescribeEventSubscriptions = DescribeEventSubscriptionsResponse
    request = getQuery service "DescribeEventSubscriptions"

data DescribeEventSubscriptionsResponse = DescribeEventSubscriptionsResponse
    { desnrsEventSubscriptionsList :: [EventSubscription]
      -- ^ A list of event subscriptions.
    , desnrsMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DescribeOrderableClusterOptions request. If this parameter is specified,
      -- the response includes only records beyond the marker, up to the value
      -- specified by MaxRecords.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeEventSubscriptionsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeEventSubscriptionsResponse"
        :| ["DescribeEventSubscriptionsResult"]
