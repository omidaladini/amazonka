{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeSnapshotAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about an attribute of a snapshot. Only one attribute
-- can be specified per call.
module Network.AWS.EC2.DescribeSnapshotAttribute where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeSnapshotAttribute :: SnapshotAttributeName
                          -- ^ The name of the EBS attribute to describe. Available attribute names:
                          -- createVolumePermission.
                          -> Text
                          -- ^ The ID of the EBS snapshot whose attribute is being described.
                          -> DescribeSnapshotAttribute
describeSnapshotAttribute p1 p2 = DescribeSnapshotAttribute
    { dsaAttribute = p1
    , dsaSnapshotId = p2
    , dsaDryRun = Nothing
    }

data DescribeSnapshotAttribute = DescribeSnapshotAttribute
    { dsaAttribute :: !SnapshotAttributeName
      -- ^ The name of the EBS attribute to describe. Available attribute names:
      -- createVolumePermission.
    , dsaDryRun :: Maybe Bool
    , dsaSnapshotId :: !Text
      -- ^ The ID of the EBS snapshot whose attribute is being described.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeSnapshotAttribute

instance AWSRequest DescribeSnapshotAttribute where
    type Er DescribeSnapshotAttribute = EC2Error
    type Rs DescribeSnapshotAttribute = DescribeSnapshotAttributeResponse
    request = getQuery service "DescribeSnapshotAttribute"

data DescribeSnapshotAttributeResponse = DescribeSnapshotAttributeResponse
    { dsarCreateVolumePermissions :: [CreateVolumePermission]
      -- ^ The list of permissions describing who can create a volume from the
      -- associated EBS snapshot. Only available if the createVolumePermission
      -- attribute is requested.
    , dsarProductCodes :: [ProductCode]
    , dsarSnapshotId :: Maybe Text
      -- ^ The ID of the snapshot whose attribute is being described.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeSnapshotAttributeResponse where
    fromXMLOptions = xmlOptions
