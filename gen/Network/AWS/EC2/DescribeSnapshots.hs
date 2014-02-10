{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeSnapshots
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about the Amazon EBS snapshots available to you.
-- Snapshots available to you include public snapshots available for any AWS
-- account to launch, private snapshots you own, and private snapshots owned
-- by another AWS account but for which you've been given explicit create
-- volume permissions.
module Network.AWS.EC2.DescribeSnapshots where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeSnapshots = DescribeSnapshots
    { dsdDryRun :: Maybe Bool
    , dsdFilters :: [Filter]
      -- ^ A list of filters used to match properties for Snapshots. For a complete
      -- reference to the available filter keys for this operation, see the Amazon
      -- EC2 API reference.
    , dsdOwnerIds :: [Text]
      -- ^ An optional list of owners by which to scope the described EBS snapshots.
      -- Valid values are: self : Snapshots owned by you AWS account ID : Snapshots
      -- owned by this account ID amazon : Snapshots owned by Amazon The values self
      -- and amazon are literals.
    , dsdRestorableByUserIds :: [Text]
      -- ^ An optional list of users. The described snapshots are scoped to only those
      -- snapshots from which these users can create volumes.
    , dsdSnapshotIds :: [Text]
      -- ^ The optional list of EBS snapshot IDs to describe.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeSnapshots

instance AWSRequest DescribeSnapshots where
    type Er DescribeSnapshots = EC2Error
    type Rs DescribeSnapshots = DescribeSnapshotsResponse
    request  = postQuery service "DescribeSnapshots"
    response = responseXML

data DescribeSnapshotsResponse = DescribeSnapshotsResponse
    { dsdrSnapshotSet :: [Snapshot]
      -- ^ The list of described EBS snapshots.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeSnapshotsResponse where
    fromXMLOptions = xmlOptions
