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
replaceNetworkAclEntry :: Text
                       -> Bool
                       -> Text
                       -> Text
                       -> RuleAction
                       -> Int
                       -> ReplaceNetworkAclEntry
replaceNetworkAclEntry p1 p2 p3 p4 p5 p6 = ReplaceNetworkAclEntry
    { rnaerCidrBlock = p1
    , rnaerEgress = p2
    , rnaerNetworkAclId = p3
    , rnaerProtocol = p4
    , rnaerRuleAction = p5
    , rnaerRuleNumber = p6
    , rnaerDryRun = Nothing
    , rnaerIcmpTypeCode = Nothing
    , rnaerPortRange = Nothing
    }

data ReplaceNetworkAclEntry = ReplaceNetworkAclEntry
    { rnaerCidrBlock :: !Text
      -- ^ The CIDR range to allow or deny, in CIDR notation (e.g., 172.16.0.0/24).
    , rnaerDryRun :: Maybe Bool
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
    request = getQuery service "ReplaceNetworkAclEntry"

data ReplaceNetworkAclEntryResponse = ReplaceNetworkAclEntryResponse
    deriving (Eq, Show, Generic)

instance FromXML ReplaceNetworkAclEntryResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot ReplaceNetworkAclEntryResponse
