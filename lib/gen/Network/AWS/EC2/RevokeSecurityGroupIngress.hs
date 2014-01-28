{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.RevokeSecurityGroupIngress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The RevokeSecurityGroupIngress operation revokes permissions from a
-- security group. The permissions used to revoke must be specified using the
-- same values used to grant the permissions. Permissions are specified by IP
-- protocol (TCP, UDP, or ICMP), the source of the request (by IP range or an
-- Amazon EC2 user-group pair), the source and destination port ranges (for
-- TCP and UDP), and the ICMP codes and types (for ICMP). Permission changes
-- are quickly propagated to instances within the security group. However,
-- depending on the number of instances in the group, a small delay might
-- occur.
module Network.AWS.EC2.RevokeSecurityGroupIngress where

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

data RevokeSecurityGroupIngress = RevokeSecurityGroupIngress
    { rsgirDryRun :: Maybe Bool
    , rsgirGroupId :: Maybe Text
      -- ^ ID of the standard (EC2) or VPC security group to modify. The group must
      -- belong to your account. Required for VPC security groups; can be used
      -- instead of GroupName for standard (EC2) security groups.
    , rsgirGroupName :: Maybe Text
      -- ^ Name of the standard (EC2) security group to modify. The group must belong
      -- to your account. Can be used instead of GroupID for standard (EC2) security
      -- groups.
    , rsgirIpPermissions :: [IpPermission]
      -- ^ List of IP permissions to revoke on the specified security group. For an IP
      -- permission to be removed, it must exactly match one of the IP permissions
      -- you specify in this list. Specifying permissions through IP permissions is
      -- the preferred way of revoking permissions since it offers more flexibility
      -- and control.
    } deriving (Eq, Show, Generic)

instance ToQuery RevokeSecurityGroupIngress

instance AWSRequest RevokeSecurityGroupIngress where
    type Er RevokeSecurityGroupIngress = EC2Error
    type Rs RevokeSecurityGroupIngress = RevokeSecurityGroupIngressResponse
    request = getQuery service "RevokeSecurityGroupIngress"

data RevokeSecurityGroupIngressResponse = RevokeSecurityGroupIngressResponse
    deriving (Eq, Show, Generic)

instance FromXML RevokeSecurityGroupIngressResponse where
    fromXMLOptions = xmlOptions
