{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ReplaceNetworkAclEntry
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Replaces an entry (i.e., rule) in a network ACL. For more information about
-- network ACLs, go to Network ACLs in the Amazon Virtual Private Cloud User
-- Guide.
module Network.AWS.EC2.ReplaceNetworkAclEntry where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data ReplaceNetworkAclEntry = ReplaceNetworkAclEntry
    { rnaerCidrBlock :: !Text
      -- ^ The CIDR range to allow or deny, in CIDR notation (e.g., 172.16.0.0/24).
    , rnaerDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , rnaerEgress :: !Bool
      -- ^ Whether this rule applies to egress traffic from the subnet (true) or
      -- ingress traffic (false).
    , rnaerIcmpTypeCode :: Maybe IcmpTypeCode
      -- ^ ICMP values.
    , rnaerNetworkAclId :: !Text
      -- ^ ID of the ACL where the entry will be replaced.
    , rnaerPortRange :: Maybe PortRange
      -- ^ Port ranges.
    , rnaerProtocol :: !Text
      -- ^ IP protocol the rule applies to. Valid Values: tcp, udp, icmp or an IP
      -- protocol number.
    , rnaerRuleAction :: !RuleAction
      -- ^ Whether to allow or deny traffic that matches the rule.
    , rnaerRuleNumber :: !Int
      -- ^ Rule number of the entry to replace.
    } deriving (Eq, Show, Generic)

instance ToQuery ReplaceNetworkAclEntry

instance AWSRequest ReplaceNetworkAclEntry where
    type Er ReplaceNetworkAclEntry = EC2Error
    type Rs ReplaceNetworkAclEntry = ReplaceNetworkAclEntryResponse
    request = v2Query service GET "ReplaceNetworkAclEntry"

data ReplaceNetworkAclEntryResponse = ReplaceNetworkAclEntryResponse
    deriving (Eq, Show, Generic)

instance FromXML ReplaceNetworkAclEntryResponse where
    fromXMLOptions = xmlOptions
