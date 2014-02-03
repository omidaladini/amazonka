{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.ModifySnapshotCopyRetentionPeriod
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies the number of days to retain automated snapshots in the
-- destination region after they are copied from the source region.
module Network.AWS.Redshift.ModifySnapshotCopyRetentionPeriod where

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
modifySnapshotCopyRetentionPeriod :: Text
                                  -> Int
                                  -> ModifySnapshotCopyRetentionPeriod
modifySnapshotCopyRetentionPeriod p1 p2 = ModifySnapshotCopyRetentionPeriod
    { mscrpmClusterIdentifier = p1
    , mscrpmRetentionPeriod = p2
    }

data ModifySnapshotCopyRetentionPeriod = ModifySnapshotCopyRetentionPeriod
    { mscrpmClusterIdentifier :: !Text
      -- ^ The unique identifier of the cluster for which you want to change the
      -- retention period for automated snapshots that are copied to a destination
      -- region. Constraints: Must be the valid name of an existing cluster that has
      -- cross-region snapshot copy enabled.
    , mscrpmRetentionPeriod :: !Int
      -- ^ The number of days to retain automated snapshots in the destination region
      -- after they are copied from the source region. If you decrease the retention
      -- period for automated snapshots that are copied to a destination region,
      -- Amazon Redshift will delete any existing automated snapshots that were
      -- copied to the destination region and that fall outside of the new retention
      -- period. Constraints: Must be at least 1 and no more than 35.
    } deriving (Eq, Show, Generic)

instance ToQuery ModifySnapshotCopyRetentionPeriod

instance AWSRequest ModifySnapshotCopyRetentionPeriod where
    type Er ModifySnapshotCopyRetentionPeriod = RedshiftError
    type Rs ModifySnapshotCopyRetentionPeriod = ModifySnapshotCopyRetentionPeriodResponse
    request = getQuery service "ModifySnapshotCopyRetentionPeriod"

data ModifySnapshotCopyRetentionPeriodResponse = ModifySnapshotCopyRetentionPeriodResponse
    { mscrpmrsCluster :: Maybe Cluster
      -- ^ Describes a cluster.
    } deriving (Eq, Show, Generic)

instance FromXML ModifySnapshotCopyRetentionPeriodResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ModifySnapshotCopyRetentionPeriodResponse"
        :| ["ModifySnapshotCopyRetentionPeriodResult"]
