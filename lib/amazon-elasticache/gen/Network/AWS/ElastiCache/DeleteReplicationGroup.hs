{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.DeleteReplicationGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeleteReplicationGroup operation deletes an existing replication group.
-- By default, this operation deletes the entire replication group, including
-- the primary cache cluster and all of the read replicas. You can optionally
-- delete only the read replicas, while retaining the primary cache cluster.
-- When you receive a successful response from this operation, Amazon
-- ElastiCache immediately begins deleting the selected resources; you cannot
-- cancel or revert this operation.
module Network.AWS.ElastiCache.DeleteReplicationGroup where

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

import Network.AWS.ElastiCache.Service
import Network.AWS.ElastiCache.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteReplicationGroup :: Text
                       -> DeleteReplicationGroup
deleteReplicationGroup p1 = DeleteReplicationGroup
    { drgmReplicationGroupId = p1
    , drgmRetainPrimaryCluster = Nothing
    }

data DeleteReplicationGroup = DeleteReplicationGroup
    { drgmReplicationGroupId :: !Text
      -- ^ The identifier for the replication group to be deleted. This parameter is
      -- not case sensitive.
    , drgmRetainPrimaryCluster :: Maybe Bool
      -- ^ If set to true, all of the read replicas will be deleted, but the primary
      -- cache cluster will be retained.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteReplicationGroup

instance AWSRequest DeleteReplicationGroup where
    type Er DeleteReplicationGroup = ElastiCacheError
    type Rs DeleteReplicationGroup = DeleteReplicationGroupResponse
    request = getQuery service "DeleteReplicationGroup"

data DeleteReplicationGroupResponse = DeleteReplicationGroupResponse
    { drgmrsReplicationGroup :: Maybe ReplicationGroup
      -- ^ Contains all of the attributes of a specific replication group.
    } deriving (Eq, Show, Generic)

instance FromXML DeleteReplicationGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeleteReplicationGroupResponse"
        :| ["DeleteReplicationGroupResult"]
