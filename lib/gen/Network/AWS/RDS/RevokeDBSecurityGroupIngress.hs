{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.RevokeDBSecurityGroupIngress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Revokes ingress from a DBSecurityGroup for previously authorized IP ranges
-- or EC2 or VPC Security Groups. Required parameters for this API are one of
-- CIDRIP, EC2SecurityGroupId for VPC, or (EC2SecurityGroupOwnerId and either
-- EC2SecurityGroupName or EC2SecurityGroupId). https://rds.amazonaws.com/
-- ?Action=RevokeDBSecurityGroupIngress &DBSecurityGroupName=mydbsecuritygroup
-- &CIDRIP=192.168.1.1%2F24 &Version=2013-05-15
-- &SignatureVersion=2&SignatureMethod=HmacSHA256
-- &Timestamp=2011-02-15T22%3A32%3A12.515Z &AWSAccessKeyId= &Signature= My new
-- DBSecurityGroup 192.168.1.1/24 revoking 621567473609 mydbsecuritygroup
-- vpc-1ab2c3d4 beecb8ac-bf5a-11de-9f9f-53d6aee22de9.
module Network.AWS.RDS.RevokeDBSecurityGroupIngress where

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

import Network.AWS.RDS.Service
import Network.AWS.RDS.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
revokeDBSecurityGroupIngress :: Text
                             -> RevokeDBSecurityGroupIngress
revokeDBSecurityGroupIngress p1 = undefined $ RevokeDBSecurityGroupIngress
    { rdbsgimDBSecurityGroupName = p1
    , rdbsgimCIDRIP = Nothing
    , rdbsgimEC2SecurityGroupId = Nothing
    , rdbsgimEC2SecurityGroupName = Nothing
    , rdbsgimEC2SecurityGroupOwnerId = Nothing
    }

data RevokeDBSecurityGroupIngress = RevokeDBSecurityGroupIngress
    { rdbsgimCIDRIP :: Maybe Text
      -- ^ The IP range to revoke access from. Must be a valid CIDR range. If CIDRIP
      -- is specified, EC2SecurityGroupName, EC2SecurityGroupId and
      -- EC2SecurityGroupOwnerId cannot be provided.
    , rdbsgimDBSecurityGroupName :: !Text
      -- ^ The name of the DB security group to revoke ingress from.
    , rdbsgimEC2SecurityGroupId :: Maybe Text
      -- ^ The id of the EC2 security group to revoke access from. For VPC DB security
      -- groups, EC2SecurityGroupId must be provided. Otherwise,
      -- EC2SecurityGroupOwnerId and either EC2SecurityGroupName or
      -- EC2SecurityGroupId must be provided.
    , rdbsgimEC2SecurityGroupName :: Maybe Text
      -- ^ The name of the EC2 security group to revoke access from. For VPC DB
      -- security groups, EC2SecurityGroupId must be provided. Otherwise,
      -- EC2SecurityGroupOwnerId and either EC2SecurityGroupName or
      -- EC2SecurityGroupId must be provided.
    , rdbsgimEC2SecurityGroupOwnerId :: Maybe Text
      -- ^ The AWS Account Number of the owner of the EC2 security group specified in
      -- the EC2SecurityGroupName parameter. The AWS Access Key ID is not an
      -- acceptable value. For VPC DB security groups, EC2SecurityGroupId must be
      -- provided. Otherwise, EC2SecurityGroupOwnerId and either
      -- EC2SecurityGroupName or EC2SecurityGroupId must be provided.
    } deriving (Eq, Show, Generic)

instance ToQuery RevokeDBSecurityGroupIngress

instance AWSRequest RevokeDBSecurityGroupIngress where
    type Er RevokeDBSecurityGroupIngress = RDSError
    type Rs RevokeDBSecurityGroupIngress = RevokeDBSecurityGroupIngressResponse
    request = getQuery service "RevokeDBSecurityGroupIngress"

data RevokeDBSecurityGroupIngressResponse = RevokeDBSecurityGroupIngressResponse
    { rdbsgimrsDBSecurityGroup :: Maybe DBSecurityGroup
      -- ^ Contains the result of a successful invocation of the following actions:
      -- DescribeDBSecurityGroups AuthorizeDBSecurityGroupIngress
      -- CreateDBSecurityGroup RevokeDBSecurityGroupIngress This data type is used
      -- as a response element in the DescribeDBSecurityGroups action.
    } deriving (Eq, Show, Generic)

instance FromXML RevokeDBSecurityGroupIngressResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "RevokeDBSecurityGroupIngressResponse"
        :| ["RevokeDBSecurityGroupIngressResult"]
