{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.ResetClusterParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets one or more parameters of the specified parameter group to their
-- default values and sets the source values of the parameters to
-- "engine-default". To reset the entire parameter group specify the
-- ResetAllParameters parameter. For parameter changes to take effect you must
-- reboot any associated clusters. https://redshift.us-east-1.amazonaws.com/
-- ?Action=ResetClusterParameterGroup &ParameterGroupName=parametergroup1
-- &Parameters.member.1.ParameterName=extra_float_digits &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121208/us-east-1/redshift/aws4_request
-- &x-amz-date=20121208T020847Z
-- &x-amz-signedheaders=content-type;host;x-amz-date Your parameter group has
-- been updated but changes won't get applied until you reboot the associated
-- Clusters. parametergroup1 625d23c1-40dc-11e2-8a25-eb010998df4e.
module Network.AWS.Redshift.ResetClusterParameterGroup where

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
resetClusterParameterGroup :: Text
                           -> AWS (Either RedshiftError ResetClusterParameterGroupResponse)
resetClusterParameterGroup p1 = undefined $ ResetClusterParameterGroup
    { rcpgmParameterGroupName = p1
    , rcpgmParameters = []
    , rcpgmResetAllParameters = Nothing
    }

data ResetClusterParameterGroup = ResetClusterParameterGroup
    { rcpgmParameterGroupName :: !Text
      -- ^ The name of the cluster parameter group to be reset.
    , rcpgmParameters :: [Parameter]
      -- ^ An array of names of parameters to be reset. If ResetAllParameters option
      -- is not used, then at least one parameter name must be supplied.
      -- Constraints: A maximum of 20 parameters can be reset in a single request.
    , rcpgmResetAllParameters :: Maybe Bool
      -- ^ If true, all parameters in the specified parameter group will be reset to
      -- their default values. Default: true.
    } deriving (Eq, Show, Generic)

instance ToQuery ResetClusterParameterGroup

instance AWSRequest ResetClusterParameterGroup where
    type Er ResetClusterParameterGroup = RedshiftError
    type Rs ResetClusterParameterGroup = ResetClusterParameterGroupResponse
    request = getQuery service "ResetClusterParameterGroup"

data ResetClusterParameterGroupResponse = ResetClusterParameterGroupResponse
    { rcpgmrsParameterGroupName :: Maybe Text
      -- ^ The name of the cluster parameter group.
    , rcpgmrsParameterGroupStatus :: Maybe Text
      -- ^ The status of the parameter group. For example, if you made a change to a
      -- parameter group name-value pair, then the change could be pending a reboot
      -- of an associated cluster.
    } deriving (Eq, Show, Generic)

instance FromXML ResetClusterParameterGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ResetClusterParameterGroupResponse"
        :| ["ResetClusterParameterGroupResult"]
