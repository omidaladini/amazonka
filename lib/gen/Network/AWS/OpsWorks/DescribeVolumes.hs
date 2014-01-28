{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DescribeVolumes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes an instance's Amazon EBS volumes. You must specify at least one
-- of the parameters. Required Permissions: To use this action, an IAM user
-- must have a Show, Deploy, or Manage permissions level for the stack, or an
-- attached policy that explicitly grants permissions. For more information on
-- user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.DescribeVolumes where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.OpsWorks.Service
import Network.AWS.OpsWorks.Types

data DescribeVolumes = DescribeVolumes
    { dvsInstanceId :: Maybe Text
      -- ^ The instance ID. If you use this parameter, DescribeVolumes returns
      -- descriptions of the volumes associated with the specified instance.
    , dvsRaidArrayId :: Maybe Text
      -- ^ The RAID array ID. If you use this parameter, DescribeVolumes returns
      -- descriptions of the volumes associated with the specified RAID array.
    , dvsStackId :: Maybe Text
      -- ^ A stack ID. The action describes the stack's registered Amazon EBS volumes.
    , dvsVolumeIds :: [Text]
      -- ^ Am array of volume IDs. If you use this parameter, DescribeVolumes returns
      -- descriptions of the specified volumes. Otherwise, it returns a description
      -- of every volume.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeVolumes

instance AWSRequest DescribeVolumes where
    type Er DescribeVolumes = OpsWorksError
    type Rs DescribeVolumes = DescribeVolumesResponse
    request  = getJSON service
    response = responseJSON

data DescribeVolumesResponse = DescribeVolumesResponse
    { dvsrsVolumes :: [Volume]
      -- ^ An array of volume IDs.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeVolumesResponse
