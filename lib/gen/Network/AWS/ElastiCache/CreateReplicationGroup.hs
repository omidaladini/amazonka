{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.CreateReplicationGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateReplicationGroup operation creates a replication group. A
-- replication group is a collection of cache clusters, where one of the
-- clusters is a read/write primary and the other clusters are read-only
-- replicas. Writes to the primary are automatically propagated to the
-- replicas. When you create a replication group, you must specify an existing
-- cache cluster that is in the primary role. When the replication group has
-- been successfully created, you can add one or more read replica replicas to
-- it, up to a total of five read replicas.
module Network.AWS.ElastiCache.CreateReplicationGroup where

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

data CreateReplicationGroup = CreateReplicationGroup
    { crgmPrimaryClusterId :: !Text
      -- ^ The identifier of the cache cluster that will serve as the primary for this
      -- replication group. This cache cluster must already exist and have a status
      -- of available.
    , crgmReplicationGroupDescription :: !Text
      -- ^ A user-specified description for the replication group.
    , crgmReplicationGroupId :: !Text
      -- ^ The replication group identifier. This parameter is stored as a lowercase
      -- string. Constraints: Must contain from 1 to 20 alphanumeric characters or
      -- hyphens. First character must be a letter. Cannot end with a hyphen or
      -- contain two consecutive hyphens.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateReplicationGroup

instance AWSRequest CreateReplicationGroup where
    type Er CreateReplicationGroup = ElastiCacheError
    type Rs CreateReplicationGroup = CreateReplicationGroupResponse
    request = getQuery service "CreateReplicationGroup"

data CreateReplicationGroupResponse = CreateReplicationGroupResponse
    { crgmrsReplicationGroup :: Maybe ReplicationGroup
      -- ^ Contains all of the attributes of a specific replication group.
    } deriving (Eq, Show, Generic)

instance FromXML CreateReplicationGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateReplicationGroupResponse"
        :| ["CreateReplicationGroupResult"]
