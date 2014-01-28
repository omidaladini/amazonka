{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeSecurityGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeSecurityGroups operation returns information about security
-- groups that you own. If you specify security group names, information about
-- those security group is returned. Otherwise, information for all security
-- group is returned. If you specify a group that does not exist, a fault is
-- returned.
module Network.AWS.EC2.DescribeSecurityGroups where

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

data DescribeSecurityGroups = DescribeSecurityGroups
    { dsgsDryRun :: Maybe Bool
    , dsgsFilters :: [Filter]
      -- ^ A list of filters used to match properties for SecurityGroups. For a
      -- complete reference to the available filter keys for this operation, see the
      -- Amazon EC2 API reference.
    , dsgsGroupIds :: [Text]
    , dsgsGroupNames :: [Text]
      -- ^ An optional list of group names that specify the Amazon EC2 security groups
      -- to describe.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeSecurityGroups

instance AWSRequest DescribeSecurityGroups where
    type Er DescribeSecurityGroups = EC2Error
    type Rs DescribeSecurityGroups = DescribeSecurityGroupsResponse
    request = getQuery service "DescribeSecurityGroups"

data DescribeSecurityGroupsResponse = DescribeSecurityGroupsResponse
    { dsgsrsSecurityGroups :: [SecurityGroup]
      -- ^ The list of described Amazon EC2 security groups.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeSecurityGroupsResponse where
    fromXMLOptions = xmlOptions
