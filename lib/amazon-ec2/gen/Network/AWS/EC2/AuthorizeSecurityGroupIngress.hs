{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.AuthorizeSecurityGroupIngress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The AuthorizeSecurityGroupIngress operation adds permissions to a security
-- group. Permissions are specified by the IP protocol (TCP, UDP or ICMP), the
-- source of the request (by IP range or an Amazon EC2 user-group pair), the
-- source and destination port ranges (for TCP and UDP), and the ICMP codes
-- and types (for ICMP). When authorizing ICMP, -1 can be used as a wildcard
-- in the type and code fields. Permission changes are propagated to instances
-- within the security group as quickly as possible. However, depending on the
-- number of instances, a small delay might occur.
module Network.AWS.EC2.AuthorizeSecurityGroupIngress where

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
authorizeSecurityGroupIngress :: AuthorizeSecurityGroupIngress
authorizeSecurityGroupIngress = AuthorizeSecurityGroupIngress
    { asgirDryRun = Nothing
    , asgirGroupId = Nothing
    , asgirGroupName = Nothing
    , asgirIpPermissions = []
    }

data AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngress
    { asgirDryRun :: Maybe Bool
    , asgirGroupId :: Maybe Text
      -- ^ ID of the standard (EC2) or VPC security group to modify. The group must
      -- belong to your account. Required for VPC security groups; can be used
      -- instead of GroupName for standard (EC2) security groups.
    , asgirGroupName :: Maybe Text
      -- ^ Name of the standard (EC2) security group to modify. The group must belong
      -- to your account. Can be used instead of GroupID for standard (EC2) security
      -- groups.
    , asgirIpPermissions :: [IpPermission]
      -- ^ List of IP permissions to authorize on the specified security group.
      -- Specifying permissions through IP permissions is the preferred way of
      -- authorizing permissions since it offers more flexibility and control.
    } deriving (Eq, Show, Generic)

instance ToQuery AuthorizeSecurityGroupIngress

instance AWSRequest AuthorizeSecurityGroupIngress where
    type Er AuthorizeSecurityGroupIngress = EC2Error
    type Rs AuthorizeSecurityGroupIngress = AuthorizeSecurityGroupIngressResponse
    request = getQuery service "AuthorizeSecurityGroupIngress"

data AuthorizeSecurityGroupIngressResponse = AuthorizeSecurityGroupIngressResponse
    deriving (Eq, Show, Generic)

instance FromXML AuthorizeSecurityGroupIngressResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot AuthorizeSecurityGroupIngressResponse
