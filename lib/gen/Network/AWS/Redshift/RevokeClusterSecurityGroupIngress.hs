{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.RevokeClusterSecurityGroupIngress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Revokes an ingress rule in an Amazon Redshift security group for a
-- previously authorized IP range or Amazon EC2 security group. To add an
-- ingress rule, see AuthorizeClusterSecurityGroupIngress. For information
-- about managing security groups, go to Amazon Redshift Cluster Security
-- Groups in the Amazon Redshift Management Guide.
-- https://redshift.us-east-1.amazonaws.com/
-- ?Action=RevokeClusterSecurityGroupIngress
-- &ClusterSecurityGroupName=securitygroup1 &CIDRIP=192.168.40.3/32
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T021606Z
-- &x-amz-signedheaders=content-type;host;x-amz-date my security group
-- securitygroup1 d8eff363-6502-11e2-a8da-655adc216806.
module Network.AWS.Redshift.RevokeClusterSecurityGroupIngress where

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

import Network.AWS.Redshift.Service
import Network.AWS.Redshift.Types

data RevokeClusterSecurityGroupIngress = RevokeClusterSecurityGroupIngress
    { rcsgimCIDRIP :: Maybe Text
      -- ^ The IP range for which to revoke access. This range must be a valid
      -- Classless Inter-Domain Routing (CIDR) block of IP addresses. If CIDRIP is
      -- specified, EC2SecurityGroupName and EC2SecurityGroupOwnerId cannot be
      -- provided.
    , rcsgimClusterSecurityGroupName :: !Text
      -- ^ The name of the security Group from which to revoke the ingress rule.
    , rcsgimEC2SecurityGroupName :: Maybe Text
      -- ^ The name of the EC2 Security Group whose access is to be revoked. If
      -- EC2SecurityGroupName is specified, EC2SecurityGroupOwnerId must also be
      -- provided and CIDRIP cannot be provided.
    , rcsgimEC2SecurityGroupOwnerId :: Maybe Text
      -- ^ The AWS account number of the owner of the security group specified in the
      -- EC2SecurityGroupName parameter. The AWS access key ID is not an acceptable
      -- value. If EC2SecurityGroupOwnerId is specified, EC2SecurityGroupName must
      -- also be provided. and CIDRIP cannot be provided. Example: 111122223333.
    } deriving (Eq, Show, Generic)

instance ToQuery RevokeClusterSecurityGroupIngress

instance AWSRequest RevokeClusterSecurityGroupIngress where
    type Er RevokeClusterSecurityGroupIngress = RedshiftError
    type Rs RevokeClusterSecurityGroupIngress = RevokeClusterSecurityGroupIngressResponse
    request = getQuery service "RevokeClusterSecurityGroupIngress"

data RevokeClusterSecurityGroupIngressResponse = RevokeClusterSecurityGroupIngressResponse
    { rcsgimrsClusterSecurityGroup :: Maybe ClusterSecurityGroup
      -- ^ Describes a security group.
    } deriving (Eq, Show, Generic)

instance FromXML RevokeClusterSecurityGroupIngressResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "RevokeClusterSecurityGroupIngressResponse"
        :| ["RevokeClusterSecurityGroupIngressResult"]
