{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeInstances operation returns information about instances that
-- you own. If you specify one or more instance IDs, Amazon EC2 returns
-- information for those instances. If you do not specify instance IDs, Amazon
-- EC2 returns information for all relevant instances. If you specify an
-- invalid instance ID, a fault is returned. If you specify an instance that
-- you do not own, it will not be included in the returned results. Recently
-- terminated instances might appear in the returned results. This interval is
-- usually less than one hour.
module Network.AWS.EC2.DescribeInstances where

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

import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeInstances :: DescribeInstances
describeInstances = DescribeInstances
    { disDryRun = Nothing
    , disFilters = []
    , disInstanceIds = []
    , disMaxResults = Nothing
    , disNextToken = Nothing
    }

data DescribeInstances = DescribeInstances
    { disDryRun :: Maybe Bool
    , disFilters :: [Filter]
      -- ^ A list of filters used to match properties for Instances. For a complete
      -- reference to the available filter keys for this operation, see the Amazon
      -- EC2 API reference.
    , disInstanceIds :: [Text]
      -- ^ An optional list of the instances to describe.
    , disMaxResults :: Maybe Int
    , disNextToken :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeInstances

instance AWSRequest DescribeInstances where
    type Er DescribeInstances = EC2Error
    type Rs DescribeInstances = DescribeInstancesResponse
    request = getQuery service "DescribeInstances"

instance AWSPager DescribeInstances where
    next rq rs
        | Just x <- disrsNextToken rs = Just $ rq { disNextToken = Just x }
        | otherwise = Nothing

data DescribeInstancesResponse = DescribeInstancesResponse
    { disrsNextToken :: Maybe Text
    , disrsReservations :: [Reservation]
      -- ^ The list of reservations containing the describes instances.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeInstancesResponse where
    fromXMLOptions = xmlOptions
