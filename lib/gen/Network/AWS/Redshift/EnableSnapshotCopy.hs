{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.EnableSnapshotCopy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables the automatic copy of snapshots from one region to another region
-- for a specified cluster.
module Network.AWS.Redshift.EnableSnapshotCopy where

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

data EnableSnapshotCopy = EnableSnapshotCopy
    { escmClusterIdentifier :: !Text
      -- ^ The unique identifier of the source cluster to copy snapshots from.
      -- Constraints: Must be the valid name of an existing cluster that does not
      -- already have cross-region snapshot copy enabled.
    , escmDestinationRegion :: !Text
      -- ^ The destination region that you want to copy snapshots to. Constraints:
      -- Must be the name of a valid region. For more information, see Regions and
      -- Endpoints in the Amazon Web Services General Reference.
    , escmRetentionPeriod :: Maybe Int
      -- ^ The number of days to retain automated snapshots in the destination region
      -- after they are copied from the source region. Default: 7. Constraints: Must
      -- be at least 1 and no more than 35.
    } deriving (Eq, Show, Generic)

instance ToQuery EnableSnapshotCopy

instance AWSRequest EnableSnapshotCopy where
    type Er EnableSnapshotCopy = RedshiftError
    type Rs EnableSnapshotCopy = EnableSnapshotCopyResponse
    request = getQuery service "EnableSnapshotCopy"

data EnableSnapshotCopyResponse = EnableSnapshotCopyResponse
    { escmrsCluster :: Maybe Cluster
      -- ^ Describes a cluster.
    } deriving (Eq, Show, Generic)

instance FromXML EnableSnapshotCopyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "EnableSnapshotCopyResponse"
        :| ["EnableSnapshotCopyResult"]
