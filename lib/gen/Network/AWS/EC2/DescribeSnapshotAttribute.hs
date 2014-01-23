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

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeSnapshotAttribute = DescribeSnapshotAttribute
    { dsarAttribute :: !SnapshotAttributeName
      -- ^ The name of the EBS attribute to describe. Available attribute names:
      -- createVolumePermission.
    , dsarDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , dsarSnapshotId :: !Text
      -- ^ The ID of the EBS snapshot whose attribute is being described.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeSnapshotAttribute

instance AWSRequest DescribeSnapshotAttribute where
    type Er DescribeSnapshotAttribute = EC2Error
    type Rs DescribeSnapshotAttribute = DescribeSnapshotAttributeResponse
    request = v2Query service GET "DescribeSnapshotAttribute"

data DescribeSnapshotAttributeResponse = DescribeSnapshotAttributeResponse
    { dsarrsCreateVolumePermissions :: [CreateVolumePermission]
      -- ^ The list of permissions describing who can create a volume from the
      -- associated EBS snapshot. Only available if the createVolumePermission
      -- attribute is requested.
    , dsarrsProductCodes :: [ProductCode]
      -- ^ FIXME: Missing documentation
    , dsarrsSnapshotId :: Maybe Text
      -- ^ The ID of the snapshot whose attribute is being described.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeSnapshotAttributeResponse where
    fromXMLOptions = xmlOptions
