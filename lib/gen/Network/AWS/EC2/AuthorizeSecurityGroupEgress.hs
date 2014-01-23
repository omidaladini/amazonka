{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.AuthorizeSecurityGroupEgress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This action applies only to security groups in a VPC; it's not supported
-- for EC2 security groups. For information about Amazon Virtual Private Cloud
-- and VPC security groups, go to the Amazon Virtual Private Cloud User Guide.
-- The action adds one or more egress rules to a VPC security group.
-- Specifically, this permits instances in a security group to send traffic to
-- either one or more destination CIDR IP address ranges, or to one or more
-- destination security groups in the same VPC. Each rule consists of the
-- protocol (e.g., TCP), plus either a CIDR range, or a source group. For the
-- TCP and UDP protocols, you must also specify the destination port or port
-- range. For the ICMP protocol, you must also specify the ICMP type and code.
-- You can use -1 as a wildcard for the ICMP type or code. Rule changes are
-- propagated to instances within the security group as quickly as possible.
-- However, a small delay might occur. Important: For VPC security groups: You
-- can have up to 50 rules total per group (covering both ingress and egress).
module Network.AWS.EC2.AuthorizeSecurityGroupEgress where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data AuthorizeSecurityGroupEgress = AuthorizeSecurityGroupEgress
    { asgerDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , asgerGroupId :: !Text
      -- ^ ID of the VPC security group to modify.
    , asgerIpPermissions :: [IpPermission]
      -- ^ List of IP permissions to authorize on the specified security group.
      -- Specifying permissions through IP permissions is the preferred way of
      -- authorizing permissions since it offers more flexibility and control.
    } deriving (Eq, Show, Generic)

instance ToQuery AuthorizeSecurityGroupEgress

instance AWSRequest AuthorizeSecurityGroupEgress where
    type Er AuthorizeSecurityGroupEgress = EC2Error
    type Rs AuthorizeSecurityGroupEgress = AuthorizeSecurityGroupEgressResponse
    request = v2Query service GET "AuthorizeSecurityGroupEgress"

data AuthorizeSecurityGroupEgressResponse = AuthorizeSecurityGroupEgressResponse
    deriving (Eq, Show, Generic)

instance FromXML AuthorizeSecurityGroupEgressResponse where
    fromXMLOptions = xmlOptions
