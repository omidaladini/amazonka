{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DescribeNotificationConfigurations
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of notification actions associated with Auto Scaling groups
-- for specified events.
module Network.AWS.AutoScaling.DescribeNotificationConfigurations where

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

import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeNotificationConfigurations :: DescribeNotificationConfigurations
describeNotificationConfigurations = DescribeNotificationConfigurations
    { dncuAutoScalingGroupNames = []
    , dncuMaxRecords = Nothing
    , dncuNextToken = Nothing
    }

data DescribeNotificationConfigurations = DescribeNotificationConfigurations
    { dncuAutoScalingGroupNames :: [ResourceName]
      -- ^ The name of the Auto Scaling group.
    , dncuMaxRecords :: Maybe Int
      -- ^ Maximum number of records to be returned.
    , dncuNextToken :: Maybe Text
      -- ^ A string that is used to mark the start of the next batch of returned
      -- results for pagination.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeNotificationConfigurations

instance AWSRequest DescribeNotificationConfigurations where
    type Er DescribeNotificationConfigurations = AutoScalingError
    type Rs DescribeNotificationConfigurations = DescribeNotificationConfigurationsResponse
    request = getQuery service "DescribeNotificationConfigurations"

instance AWSPager DescribeNotificationConfigurations where
    next rq rs
        | Just x <- dncursNextToken rs = Just $ rq { dncuNextToken = Just x }
        | otherwise = Nothing

data DescribeNotificationConfigurationsResponse = DescribeNotificationConfigurationsResponse
    { dncursNextToken :: Maybe Text
      -- ^ A string that is used to mark the start of the next batch of returned
      -- results for pagination.
    , dncursNotificationConfigurations :: [NotificationConfiguration]
      -- ^ The list of notification configurations.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeNotificationConfigurationsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeNotificationConfigurationsResponse"
        :| ["DescribeNotificationConfigurationsResult"]
