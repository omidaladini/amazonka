{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DisableSnapshotCopy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Disables the automatic copying of snapshots from one region to another
-- region for a specified cluster.
module Network.AWS.Redshift.DisableSnapshotCopy where

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
disableSnapshotCopy :: Text
                    -> DisableSnapshotCopy
disableSnapshotCopy p1 = DisableSnapshotCopy
    { dscmClusterIdentifier = p1
    }

data DisableSnapshotCopy = DisableSnapshotCopy
    { dscmClusterIdentifier :: !Text
      -- ^ The unique identifier of the source cluster that you want to disable
      -- copying of snapshots to a destination region. Constraints: Must be the
      -- valid name of an existing cluster that has cross-region snapshot copy
      -- enabled.
    } deriving (Eq, Show, Generic)

instance ToQuery DisableSnapshotCopy

instance AWSRequest DisableSnapshotCopy where
    type Er DisableSnapshotCopy = RedshiftError
    type Rs DisableSnapshotCopy = DisableSnapshotCopyResponse
    request = getQuery service "DisableSnapshotCopy"

data DisableSnapshotCopyResponse = DisableSnapshotCopyResponse
    { dscmrsCluster :: Maybe Cluster
      -- ^ Describes a cluster.
    } deriving (Eq, Show, Generic)

instance FromXML DisableSnapshotCopyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DisableSnapshotCopyResponse"
        :| ["DisableSnapshotCopyResult"]
