{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.ModifyClusterSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies a cluster subnet group to include the specified list of VPC
-- subnets. The operation replaces the existing list of subnets with the new
-- list of subnets.
module Network.AWS.Redshift.ModifyClusterSubnetGroup where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
modifyClusterSubnetGroup :: Text
                         -> [Text]
                         -> ModifyClusterSubnetGroup
modifyClusterSubnetGroup p1 p2 = ModifyClusterSubnetGroup
    { mcsgmClusterSubnetGroupName = p1
    , mcsgmSubnetIds = p2
    , mcsgmDescription = Nothing
    }

data ModifyClusterSubnetGroup = ModifyClusterSubnetGroup
    { mcsgmClusterSubnetGroupName :: !Text
      -- ^ The name of the subnet group to be modified.
    , mcsgmDescription :: Maybe Text
      -- ^ A text description of the subnet group to be modified.
    , mcsgmSubnetIds :: [Text]
      -- ^ An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a
      -- single request.
    } deriving (Eq, Show, Generic)

instance ToQuery ModifyClusterSubnetGroup

instance AWSRequest ModifyClusterSubnetGroup where
    type Er ModifyClusterSubnetGroup = RedshiftError
    type Rs ModifyClusterSubnetGroup = ModifyClusterSubnetGroupResponse
    request = getQuery service "ModifyClusterSubnetGroup"

data ModifyClusterSubnetGroupResponse = ModifyClusterSubnetGroupResponse
    { mcsgmrsClusterSubnetGroup :: Maybe ClusterSubnetGroup
      -- ^ Describes a subnet group.
    } deriving (Eq, Show, Generic)

instance FromXML ModifyClusterSubnetGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ModifyClusterSubnetGroupResponse"
        :| ["ModifyClusterSubnetGroupResult"]
