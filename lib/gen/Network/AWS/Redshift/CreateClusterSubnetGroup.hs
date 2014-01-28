{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.CreateClusterSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new Amazon Redshift subnet group. You must provide a list of one
-- or more subnets in your existing Amazon Virtual Private Cloud (Amazon VPC)
-- when creating Amazon Redshift subnet group. For information about subnet
-- groups, go to Amazon Redshift Cluster Subnet Groups in the Amazon Redshift
-- Management Guide. https://redshift.us-east-1.amazonaws.com/
-- ?Action=CreateClusterSubnetGroup &ClusterSubnetGroupName=mysubnetgroup1
-- &Description=My subnet group 1 &SubnetIds.member.1=subnet-756a591f
-- &SubnetIds.member.1=subnet-716a591b &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130129/us-east-1/redshift/aws4_request
-- &x-amz-date=20130129T192820Z
-- &x-amz-signedheaders=content-type;host;x-amz-date vpc-796a5913 My subnet
-- group 1 mysubnetgroup1 Complete Active subnet-756a591f us-east-1c
-- 0a60660f-6a4a-11e2-aad2-71d00c36728e.
module Network.AWS.Redshift.CreateClusterSubnetGroup where

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

data CreateClusterSubnetGroup = CreateClusterSubnetGroup
    { ccsgmClusterSubnetGroupName :: !Text
      -- ^ The name for the subnet group. Amazon Redshift stores the value as a
      -- lowercase string. Constraints: Must contain no more than 255 alphanumeric
      -- characters or hyphens. Must not be "Default". Must be unique for all subnet
      -- groups that are created by your AWS account. Example: examplesubnetgroup.
    , ccsgmDescription :: !Text
      -- ^ A description for the subnet group.
    , ccsgmSubnetIds :: [Text]
      -- ^ An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a
      -- single request.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateClusterSubnetGroup

instance AWSRequest CreateClusterSubnetGroup where
    type Er CreateClusterSubnetGroup = RedshiftError
    type Rs CreateClusterSubnetGroup = CreateClusterSubnetGroupResponse
    request = getQuery service "CreateClusterSubnetGroup"

data CreateClusterSubnetGroupResponse = CreateClusterSubnetGroupResponse
    { ccsgmrsClusterSubnetGroup :: Maybe ClusterSubnetGroup
      -- ^ Describes a subnet group.
    } deriving (Eq, Show, Generic)

instance FromXML CreateClusterSubnetGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateClusterSubnetGroupResponse"
        :| ["CreateClusterSubnetGroupResult"]
