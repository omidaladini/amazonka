{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DeleteClusterSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified manual snapshot. The snapshot must be in the
-- "available" state, with no other users authorized to access the snapshot.
-- Unlike automated snapshots, manual snapshots are retained even after you
-- delete your cluster. Amazon Redshift does not delete your manual snapshots.
-- You must delete manual snapshot explicitly to avoid getting charged. If
-- other accounts are authorized to access the snapshot, you must revoke all
-- of the authorizations before you can delete the snapshot.
-- https://redshift.us-east-1.amazonaws.com/ ?Action=DeleteClusterSnapshot
-- &SnapshotIdentifier=snapshot-1234 &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121208/us-east-1/redshift/aws4_request
-- &x-amz-date=20121208T005225Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 2012-12-07T23:31:02.372Z
-- 5439 snapshot-1234 deleted 2012-12-06T23:09:01.475Z manual 1.0 us-east-1a
-- examplecluster masteruser dw.hs1.xlarge mydb 3
-- 88a31de4-40d1-11e2-8a25-eb010998df4e.
module Network.AWS.Redshift.DeleteClusterSnapshot where

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
deleteClusterSnapshot :: Text
                      -> AWS (Either RedshiftError DeleteClusterSnapshotResponse)
deleteClusterSnapshot p1 = undefined $ DeleteClusterSnapshot
    { dcsmSnapshotIdentifier = p1
    , dcsmSnapshotClusterIdentifier = Nothing
    }

data DeleteClusterSnapshot = DeleteClusterSnapshot
    { dcsmSnapshotClusterIdentifier :: Maybe Text
      -- ^ The unique identifier of the cluster the snapshot was created from. This
      -- parameter is required if your IAM user has a policy containing a snapshot
      -- resource element that specifies anything other than * for the cluster name.
      -- Constraints: Must be the name of valid cluster.
    , dcsmSnapshotIdentifier :: !Text
      -- ^ The unique identifier of the manual snapshot to be deleted. Constraints:
      -- Must be the name of an existing snapshot that is in the available state.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteClusterSnapshot

instance AWSRequest DeleteClusterSnapshot where
    type Er DeleteClusterSnapshot = RedshiftError
    type Rs DeleteClusterSnapshot = DeleteClusterSnapshotResponse
    request = getQuery service "DeleteClusterSnapshot"

data DeleteClusterSnapshotResponse = DeleteClusterSnapshotResponse
    { dcsmrsSnapshot :: Maybe Snapshot
      -- ^ Describes a snapshot.
    } deriving (Eq, Show, Generic)

instance FromXML DeleteClusterSnapshotResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeleteClusterSnapshotResponse"
        :| ["DeleteClusterSnapshotResult"]
