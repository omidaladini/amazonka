{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.CreateClusterSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a manual snapshot of the specified cluster. The cluster must be in
-- the "available" state. For more information about working with snapshots,
-- go to Amazon Redshift Snapshots in the Amazon Redshift Management Guide.
-- https://redshift.us-east-1.amazonaws.com/ ?Action=CreateClusterSnapshot
-- &ClusterIdentifier=examplecluster &SnapshotIdentifier=snapshot-1234
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T010824Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 5439 my-snapshot-123
-- creating manual 1.0 2013-01-23T01:08:29.142Z 2 dev 2013-01-22T19:23:59.368Z
-- us-east-1c dw.hs1.xlarge examplecluster adminuser
-- 65baef14-64f9-11e2-bea9-49e0ce183f07.
module Network.AWS.Redshift.CreateClusterSnapshot where

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
createClusterSnapshot :: Text
                      -> Text
                      -> CreateClusterSnapshot
createClusterSnapshot p1 p2 = CreateClusterSnapshot
    { ccsmClusterIdentifier = p1
    , ccsmSnapshotIdentifier = p2
    }

data CreateClusterSnapshot = CreateClusterSnapshot
    { ccsmClusterIdentifier :: !Text
      -- ^ The cluster identifier for which you want a snapshot.
    , ccsmSnapshotIdentifier :: !Text
      -- ^ A unique identifier for the snapshot that you are requesting. This
      -- identifier must be unique for all snapshots within the AWS account.
      -- Constraints: Cannot be null, empty, or blank Must contain from 1 to 255
      -- alphanumeric characters or hyphens First character must be a letter Cannot
      -- end with a hyphen or contain two consecutive hyphens Example:
      -- my-snapshot-id.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateClusterSnapshot

instance AWSRequest CreateClusterSnapshot where
    type Er CreateClusterSnapshot = RedshiftError
    type Rs CreateClusterSnapshot = CreateClusterSnapshotResponse
    request = getQuery service "CreateClusterSnapshot"

data CreateClusterSnapshotResponse = CreateClusterSnapshotResponse
    { ccsmrsSnapshot :: Maybe Snapshot
      -- ^ Describes a snapshot.
    } deriving (Eq, Show, Generic)

instance FromXML CreateClusterSnapshotResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateClusterSnapshotResponse"
        :| ["CreateClusterSnapshotResult"]
