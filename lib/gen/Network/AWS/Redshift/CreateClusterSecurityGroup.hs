{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.CreateClusterSecurityGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new Amazon Redshift security group. You use security groups to
-- control access to non-VPC clusters. For information about managing security
-- groups, go to Amazon Redshift Cluster Security Groups in the Amazon
-- Redshift Management Guide. https://redshift.us-east-1.amazonaws.com/
-- ?Action=CreateClusterSecurityGroup &ClusterSecurityGroupName=securitygroup1
-- &Description=my security group &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T005817Z
-- &x-amz-signedheaders=content-type;host;x-amz-date my security group
-- securitygroup1 f9ee270f-64f7-11e2-a8da-655adc216806.
module Network.AWS.Redshift.CreateClusterSecurityGroup where

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

-- | Convenience method utilising default fields where applicable.
createClusterSecurityGroup :: Text
                           -> Text
                           -> AWS (Either RedshiftError CreateClusterSecurityGroupResponse)
createClusterSecurityGroup p1 p2 = undefined $ CreateClusterSecurityGroup
    { ccsgnClusterSecurityGroupName = p1
    , ccsgnDescription = p2
    }

data CreateClusterSecurityGroup = CreateClusterSecurityGroup
    { ccsgnClusterSecurityGroupName :: !Text
      -- ^ The name for the security group. Amazon Redshift stores the value as a
      -- lowercase string. Constraints: Must contain no more than 255 alphanumeric
      -- characters or hyphens. Must not be "Default". Must be unique for all
      -- security groups that are created by your AWS account. Example:
      -- examplesecuritygroup.
    , ccsgnDescription :: !Text
      -- ^ A description for the security group.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateClusterSecurityGroup

instance AWSRequest CreateClusterSecurityGroup where
    type Er CreateClusterSecurityGroup = RedshiftError
    type Rs CreateClusterSecurityGroup = CreateClusterSecurityGroupResponse
    request = getQuery service "CreateClusterSecurityGroup"

data CreateClusterSecurityGroupResponse = CreateClusterSecurityGroupResponse
    { ccsgnrsClusterSecurityGroup :: Maybe ClusterSecurityGroup
      -- ^ Describes a security group.
    } deriving (Eq, Show, Generic)

instance FromXML CreateClusterSecurityGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateClusterSecurityGroupResponse"
        :| ["CreateClusterSecurityGroupResult"]
