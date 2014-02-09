{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateNetworkAclEntry
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an entry (i.e., rule) in a network ACL with a rule number you
-- specify. Each network ACL has a set of numbered ingress rules and a
-- separate set of numbered egress rules. When determining whether a packet
-- should be allowed in or out of a subnet associated with the ACL, Amazon VPC
-- processes the entries in the ACL according to the rule numbers, in
-- ascending order. Important: We recommend that you leave room between the
-- rules (e.g., 100, 110, 120, etc.), and not number them sequentially (101,
-- 102, 103, etc.). This allows you to easily add a new rule between existing
-- ones without having to renumber the rules. After you add an entry, you
-- can't modify it; you must either replace it, or create a new entry and
-- delete the old one. For more information about network ACLs, go to Network
-- ACLs in the Amazon Virtual Private Cloud User Guide.
module Network.AWS.EC2.CreateNetworkAclEntry where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createNetworkAclEntry :: Text
                      -- ^ The CIDR range to allow or deny, in CIDR notation (e.g., 172.16.0.0/24).
                      -> Bool
                      -- ^ Whether this rule applies to egress traffic from the subnet (true) or
                      -- ingress traffic to the subnet (false).
                      -> Text
                      -- ^ ID of the ACL where the entry will be created.
                      -> Text
                      -- ^ IP protocol the rule applies to. Valid Values: tcp, udp, icmp or an IP
                      -- protocol number.
                      -> RuleAction
                      -- ^ Whether to allow or deny traffic that matches the rule.
                      -> Int
                      -- ^ Rule number to assign to the entry (e.g., 100). ACL entries are processed
                      -- in ascending order by rule number.
                      -> CreateNetworkAclEntry
createNetworkAclEntry p1 p2 p3 p4 p5 p6 = CreateNetworkAclEntry
    { cnaeCidrBlock = p1
    , cnaeEgress = p2
    , cnaeNetworkAclId = p3
    , cnaeProtocol = p4
    , cnaeRuleAction = p5
    , cnaeRuleNumber = p6
    , cnaeDryRun = Nothing
    , cnaeIcmpTypeCode = Nothing
    , cnaePortRange = Nothing
    }

data CreateNetworkAclEntry = CreateNetworkAclEntry
    { cnaeCidrBlock :: !Text
      -- ^ The CIDR range to allow or deny, in CIDR notation (e.g., 172.16.0.0/24).
    , cnaeDryRun :: Maybe Bool
    , cnaeEgress :: !Bool
      -- ^ Whether this rule applies to egress traffic from the subnet (true) or
      -- ingress traffic to the subnet (false).
    , cnaeIcmpTypeCode :: Maybe IcmpTypeCode
      -- ^ ICMP values.
    , cnaeNetworkAclId :: !Text
      -- ^ ID of the ACL where the entry will be created.
    , cnaePortRange :: Maybe PortRange
      -- ^ Port ranges.
    , cnaeProtocol :: !Text
      -- ^ IP protocol the rule applies to. Valid Values: tcp, udp, icmp or an IP
      -- protocol number.
    , cnaeRuleAction :: !RuleAction
      -- ^ Whether to allow or deny traffic that matches the rule.
    , cnaeRuleNumber :: !Int
      -- ^ Rule number to assign to the entry (e.g., 100). ACL entries are processed
      -- in ascending order by rule number.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateNetworkAclEntry

instance AWSRequest CreateNetworkAclEntry where
    type Er CreateNetworkAclEntry = EC2Error
    type Rs CreateNetworkAclEntry = CreateNetworkAclEntryResponse
    request  = postQuery service "CreateNetworkAclEntry"
    response = responseXML

data CreateNetworkAclEntryResponse = CreateNetworkAclEntryResponse
    deriving (Eq, Show, Generic)

instance FromXML CreateNetworkAclEntryResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateNetworkAclEntryResponse"
