{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.RevokeSecurityGroupEgress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This action applies only to security groups in a VPC. It doesn't work with
-- EC2 security groups. For information about Amazon Virtual Private Cloud and
-- VPC security groups, go to the Amazon Virtual Private Cloud User Guide. The
-- action removes one or more egress rules from a VPC security group. The
-- values that you specify in the revoke request (e.g., ports, etc.) must
-- match the existing rule's values in order for the rule to be revoked. Each
-- rule consists of the protocol, and the CIDR range or destination security
-- group. For the TCP and UDP protocols, you must also specify the destination
-- port or range of ports. For the ICMP protocol, you must also specify the
-- ICMP type and code. Rule changes are propagated to instances within the
-- security group as quickly as possible. However, a small delay might occur.
module Network.AWS.EC2.RevokeSecurityGroupEgress where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
revokeSecurityGroupEgress :: Text
                          -> RevokeSecurityGroupEgress
revokeSecurityGroupEgress p1 = RevokeSecurityGroupEgress
    { rsgerGroupId = p1
    , rsgerDryRun = Nothing
    , rsgerIpPermissions = []
    }

data RevokeSecurityGroupEgress = RevokeSecurityGroupEgress
    { rsgerDryRun :: Maybe Bool
    , rsgerGroupId :: !Text
      -- ^ ID of the VPC security group to modify.
    , rsgerIpPermissions :: [IpPermission]
      -- ^ List of IP permissions to authorize on the specified security group.
      -- Specifying permissions through IP permissions is the preferred way of
      -- authorizing permissions since it offers more flexibility and control.
    } deriving (Eq, Show, Generic)

instance ToQuery RevokeSecurityGroupEgress

instance AWSRequest RevokeSecurityGroupEgress where
    type Er RevokeSecurityGroupEgress = EC2Error
    type Rs RevokeSecurityGroupEgress = RevokeSecurityGroupEgressResponse
    request = getQuery service "RevokeSecurityGroupEgress"

data RevokeSecurityGroupEgressResponse = RevokeSecurityGroupEgressResponse
    deriving (Eq, Show, Generic)

instance FromXML RevokeSecurityGroupEgressResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RevokeSecurityGroupEgressResponse"
