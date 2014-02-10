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

import Network.AWS.Core
import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

data DescribeNotificationConfigurations = DescribeNotificationConfigurations
    { dnctdAutoScalingGroupNames :: [ResourceName]
      -- ^ The name of the Auto Scaling group.
    , dnctdMaxRecords :: Maybe Int
      -- ^ Maximum number of records to be returned.
    , dnctdNextToken :: Maybe Text
      -- ^ A string that is used to mark the start of the next batch of returned
      -- results for pagination.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeNotificationConfigurations

instance AWSRequest DescribeNotificationConfigurations where
    type Er DescribeNotificationConfigurations = AutoScalingError
    type Rs DescribeNotificationConfigurations = DescribeNotificationConfigurationsResponse
    request  = postQuery service "DescribeNotificationConfigurations"
    response = responseXML

instance AWSPager DescribeNotificationConfigurations where
    next rq rs
        | Just x <- dnctdrNextToken rs = Just $ rq { dnctdNextToken = Just x }
        | otherwise = Nothing

data DescribeNotificationConfigurationsResponse = DescribeNotificationConfigurationsResponse
    { dnctdrNextToken :: Maybe Text
      -- ^ A string that is used to mark the start of the next batch of returned
      -- results for pagination.
    , dnctdrNotificationConfigurations :: [NotificationConfiguration]
      -- ^ The list of notification configurations.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeNotificationConfigurationsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeNotificationConfigurationsResponse"
        :| ["DescribeNotificationConfigurationsResult"]
