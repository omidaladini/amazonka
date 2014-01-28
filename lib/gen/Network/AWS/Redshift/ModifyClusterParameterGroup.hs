{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.ModifyClusterParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies the parameters of a parameter group. For more information about
-- managing parameter groups, go to Amazon Redshift Parameter Groups in the
-- Amazon Redshift Management Guide. https://redshift.us-east-1.amazonaws.com/
-- ?Action=ModifyClusterParameterGroup &ParameterGroupName=parametergroup1
-- &Parameters.member.1.ParameterName=extra_float_digits
-- &Parameters.member.1.ParameterValue=2
-- &Parameters.member.2.ParameterName=wlm_json_configuration
-- &Parameters.member.2.ParameterValue=[{"user_group":["example_user_group1"],"query_group":["example_query_group1"],"query_concurrency":7},{"query_concurrency":5}]
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121208/us-east-1/redshift/aws4_request
-- &x-amz-date=20121208T022525Z
-- &x-amz-signedheaders=content-type;host;x-amz-date Your parameter group has
-- been updated but changes won't get applied until you reboot the associated
-- Clusters. parametergroup1 86e64043-40de-11e2-8a25-eb010998df4e.
module Network.AWS.Redshift.ModifyClusterParameterGroup where

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

data ModifyClusterParameterGroup = ModifyClusterParameterGroup
    { mcpgmParameterGroupName :: !Text
      -- ^ The name of the parameter group to be modified.
    , mcpgmParameters :: [Parameter]
      -- ^ An array of parameters to be modified. A maximum of 20 parameters can be
      -- modified in a single request. For each parameter to be modified, you must
      -- supply at least the parameter name and parameter value; other name-value
      -- pairs of the parameter are optional.
    } deriving (Eq, Show, Generic)

instance ToQuery ModifyClusterParameterGroup

instance AWSRequest ModifyClusterParameterGroup where
    type Er ModifyClusterParameterGroup = RedshiftError
    type Rs ModifyClusterParameterGroup = ModifyClusterParameterGroupResponse
    request = getQuery service "ModifyClusterParameterGroup"

data ModifyClusterParameterGroupResponse = ModifyClusterParameterGroupResponse
    { mcpgmrsParameterGroupName :: Maybe Text
      -- ^ The name of the cluster parameter group.
    , mcpgmrsParameterGroupStatus :: Maybe Text
      -- ^ The status of the parameter group. For example, if you made a change to a
      -- parameter group name-value pair, then the change could be pending a reboot
      -- of an associated cluster.
    } deriving (Eq, Show, Generic)

instance FromXML ModifyClusterParameterGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ModifyClusterParameterGroupResponse"
        :| ["ModifyClusterParameterGroupResult"]
