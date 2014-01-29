{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.AuthorizeDBSecurityGroupIngress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables ingress to a DBSecurityGroup using one of two forms of
-- authorization. First, EC2 or VPC security groups can be added to the
-- DBSecurityGroup if the application using the database is running on EC2 or
-- VPC instances. Second, IP ranges are available if the application accessing
-- your database is running on the Internet. Required parameters for this API
-- are one of CIDR range, EC2SecurityGroupId for VPC, or
-- (EC2SecurityGroupOwnerId and either EC2SecurityGroupName or
-- EC2SecurityGroupId for non-VPC). You cannot authorize ingress from an EC2
-- security group in one Region to an Amazon RDS DB instance in another. You
-- cannot authorize ingress from a VPC security group in one VPC to an Amazon
-- RDS DB instance in another. For an overview of CIDR ranges, go to the
-- Wikipedia Tutorial. https://rds.amazonaws.com/ ?CIDRIP=192.168.1.1%2F24
-- &DBSecurityGroupName=mydbsecuritygroup &Version=2013-05-15
-- &Action=AuthorizeDBSecurityGroupIngress &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-02-15T17%3A10%3A50.274Z
-- &AWSAccessKeyId= &Signature= My new DBSecurityGroup 192.168.1.1/24
-- authorizing 621567473609 mydbsecuritygroup vpc-1ab2c3d4
-- d9799197-bf2d-11de-b88d-993294bf1c81.
module Network.AWS.RDS.AuthorizeDBSecurityGroupIngress where

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
authorizeDBSecurityGroupIngress :: Text
                                -> AuthorizeDBSecurityGroupIngress
authorizeDBSecurityGroupIngress p1 = undefined $ AuthorizeDBSecurityGroupIngress
    { adbsgimDBSecurityGroupName = p1
    , adbsgimCIDRIP = Nothing
    , adbsgimEC2SecurityGroupId = Nothing
    , adbsgimEC2SecurityGroupName = Nothing
    , adbsgimEC2SecurityGroupOwnerId = Nothing
    }

data AuthorizeDBSecurityGroupIngress = AuthorizeDBSecurityGroupIngress
    { adbsgimCIDRIP :: Maybe Text
      -- ^ The IP range to authorize.
    , adbsgimDBSecurityGroupName :: !Text
      -- ^ The name of the DB security group to add authorization to.
    , adbsgimEC2SecurityGroupId :: Maybe Text
      -- ^ Id of the EC2 security group to authorize. For VPC DB security groups,
      -- EC2SecurityGroupId must be provided. Otherwise, EC2SecurityGroupOwnerId and
      -- either EC2SecurityGroupName or EC2SecurityGroupId must be provided.
    , adbsgimEC2SecurityGroupName :: Maybe Text
      -- ^ Name of the EC2 security group to authorize. For VPC DB security groups,
      -- EC2SecurityGroupId must be provided. Otherwise, EC2SecurityGroupOwnerId and
      -- either EC2SecurityGroupName or EC2SecurityGroupId must be provided.
    , adbsgimEC2SecurityGroupOwnerId :: Maybe Text
      -- ^ AWS Account Number of the owner of the EC2 security group specified in the
      -- EC2SecurityGroupName parameter. The AWS Access Key ID is not an acceptable
      -- value. For VPC DB security groups, EC2SecurityGroupId must be provided.
      -- Otherwise, EC2SecurityGroupOwnerId and either EC2SecurityGroupName or
      -- EC2SecurityGroupId must be provided.
    } deriving (Eq, Show, Generic)

instance ToQuery AuthorizeDBSecurityGroupIngress

instance AWSRequest AuthorizeDBSecurityGroupIngress where
    type Er AuthorizeDBSecurityGroupIngress = RDSError
    type Rs AuthorizeDBSecurityGroupIngress = AuthorizeDBSecurityGroupIngressResponse
    request = getQuery service "AuthorizeDBSecurityGroupIngress"

data AuthorizeDBSecurityGroupIngressResponse = AuthorizeDBSecurityGroupIngressResponse
    { adbsgimrsDBSecurityGroup :: Maybe DBSecurityGroup
      -- ^ Contains the result of a successful invocation of the following actions:
      -- DescribeDBSecurityGroups AuthorizeDBSecurityGroupIngress
      -- CreateDBSecurityGroup RevokeDBSecurityGroupIngress This data type is used
      -- as a response element in the DescribeDBSecurityGroups action.
    } deriving (Eq, Show, Generic)

instance FromXML AuthorizeDBSecurityGroupIngressResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "AuthorizeDBSecurityGroupIngressResponse"
        :| ["AuthorizeDBSecurityGroupIngressResult"]
