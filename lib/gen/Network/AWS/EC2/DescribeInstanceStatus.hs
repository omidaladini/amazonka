{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeInstanceStatus
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the status of an Amazon Elastic Compute Cloud (Amazon EC2)
-- instance. Instance status provides information about two types of scheduled
-- events for an instance that may require your attention: Scheduled Reboot:
-- When Amazon EC2 determines that an instance must be rebooted, the
-- instance's status will return one of two event codes: system-reboot or
-- instance-reboot. System reboot commonly occurs if certain maintenance or
-- upgrade operations require a reboot of the underlying host that supports an
-- instance. Instance reboot commonly occurs if the instance must be rebooted,
-- rather than the underlying host. Rebooting events include a scheduled start
-- and end time. Scheduled Retirement: When Amazon EC2 determines that an
-- instance must be shut down, the instance's status will return an event code
-- called instance-retirement. Retirement commonly occurs when the underlying
-- host is degraded and must be replaced. Retirement events include a
-- scheduled start and end time. You're also notified by email if one of your
-- instances is set to retiring. The email message indicates when your
-- instance will be permanently retired. If your instance is permanently
-- retired, it will not be restarted. You can avoid retirement by manually
-- restarting your instance when its event code is instance-retirement. This
-- ensures that your instance is started on a healthy host.
-- DescribeInstanceStatus returns information only for instances in the
-- running state. You can filter the results to return information only about
-- instances that match criteria you specify. For example, you could get
-- information about instances in a specific Availability Zone. You can
-- specify multiple values for a filter (e.g., more than one Availability
-- Zone). An instance must match at least one of the specified values for it
-- to be included in the results. You can specify multiple filters. An
-- instance must match all the filters for it to be included in the results.
-- If there's no match, no special message is returned; the response is simply
-- empty. You can use wildcards with the filter values: * matches zero or more
-- characters, and ? matches exactly one character. You can escape special
-- characters using a backslash before the character. For example, a value of
-- \*amazon\?\\ searches for the literal string *amazon?\. The following
-- filters are available: availability-zone - Filter on an instance's
-- availability zone. instance-state-name - Filter on the intended state of
-- the instance, e.g., running. instance-state-code - Filter on the intended
-- state code of the instance, e.g., 16.
module Network.AWS.EC2.DescribeInstanceStatus where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeInstanceStatus = DescribeInstanceStatus
    { disDryRun :: Maybe Bool
    , disFilters :: [Filter]
      -- ^ The list of filters to limit returned results.
    , disIncludeAllInstances :: Maybe Bool
    , disInstanceIds :: [Text]
      -- ^ The list of instance IDs. If not specified, all instances are described.
    , disMaxResults :: Maybe Int
      -- ^ The maximum number of paginated instance items per response.
    , disNextToken :: Maybe Text
      -- ^ A string specifying the next paginated set of results to return.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeInstanceStatus

instance AWSRequest DescribeInstanceStatus where
    type Er DescribeInstanceStatus = EC2Error
    type Rs DescribeInstanceStatus = DescribeInstanceStatusResponse
    request = getQuery service "DescribeInstanceStatus"

instance AWSPager DescribeInstanceStatus where
    next rq rs
        | Just x <- disrNextToken rs = Just $ rq { disNextToken = Just x }
        | otherwise = Nothing

data DescribeInstanceStatusResponse = DescribeInstanceStatusResponse
    { disrInstanceStatuses :: [InstanceStatus]
      -- ^ Collection of instance statuses describing the state of the requested
      -- instances.
    , disrNextToken :: Maybe Text
      -- ^ A string specifying the next paginated set of results to return.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeInstanceStatusResponse where
    fromXMLOptions = xmlOptions
