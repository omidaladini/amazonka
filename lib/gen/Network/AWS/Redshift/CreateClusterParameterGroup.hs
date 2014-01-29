{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.CreateClusterParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an Amazon Redshift parameter group. Creating parameter groups is
-- independent of creating clusters. You can associate a cluster with a
-- parameter group when you create the cluster. You can also associate an
-- existing cluster with a parameter group after the cluster is created by
-- using ModifyCluster. Parameters in the parameter group define specific
-- behavior that applies to the databases you create on the cluster. For more
-- information about managing parameter groups, go to Amazon Redshift
-- Parameter Groups in the Amazon Redshift Management Guide.
-- https://redshift.us-east-1.amazonaws.com/
-- ?Action=CreateClusterParameterGroup &Description=description my parameter
-- group &ParameterGroupFamily=redshift-1.0
-- &ParameterGroupName=parametergroup1 &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T002544Z
-- &x-amz-signedheaders=content-type;host;x-amz-date redshift-1.0 description
-- my parameter group parametergroup1 6d6df847-64f3-11e2-bea9-49e0ce183f07.
module Network.AWS.Redshift.CreateClusterParameterGroup where

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
createClusterParameterGroup :: Text
                            -> Text
                            -> Text
                            -> AWS (Either RedshiftError CreateClusterParameterGroupResponse)
createClusterParameterGroup p1 p2 p3 = undefined $ CreateClusterParameterGroup
    { ccpgmDescription = p1
    , ccpgmParameterGroupFamily = p2
    , ccpgmParameterGroupName = p3
    }

data CreateClusterParameterGroup = CreateClusterParameterGroup
    { ccpgmDescription :: !Text
      -- ^ A description of the parameter group.
    , ccpgmParameterGroupFamily :: !Text
      -- ^ The Amazon Redshift engine version to which the cluster parameter group
      -- applies. The cluster engine version determines the set of parameters. To
      -- get a list of valid parameter group family names, you can call
      -- DescribeClusterParameterGroups. By default, Amazon Redshift returns a list
      -- of all the parameter groups that are owned by your AWS account, including
      -- the default parameter groups for each Amazon Redshift engine version. The
      -- parameter group family names associated with the default parameter groups
      -- provide you the valid values. For example, a valid family name is
      -- "redshift-1.0".
    , ccpgmParameterGroupName :: !Text
      -- ^ The name of the cluster parameter group. Constraints: Must be 1 to 255
      -- alphanumeric characters or hyphens First character must be a letter. Cannot
      -- end with a hyphen or contain two consecutive hyphens. Must be unique
      -- withing your AWS account. This value is stored as a lower-case string.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateClusterParameterGroup

instance AWSRequest CreateClusterParameterGroup where
    type Er CreateClusterParameterGroup = RedshiftError
    type Rs CreateClusterParameterGroup = CreateClusterParameterGroupResponse
    request = getQuery service "CreateClusterParameterGroup"

data CreateClusterParameterGroupResponse = CreateClusterParameterGroupResponse
    { ccpgmrsClusterParameterGroup :: Maybe ClusterParameterGroup
      -- ^ Describes a parameter group.
    } deriving (Eq, Show, Generic)

instance FromXML CreateClusterParameterGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateClusterParameterGroupResponse"
        :| ["CreateClusterParameterGroupResult"]
