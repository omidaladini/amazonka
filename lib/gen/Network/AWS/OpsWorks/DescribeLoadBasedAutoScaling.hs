{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DescribeLoadBasedAutoScaling
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes load-based auto scaling configurations for specified layers. You
-- must specify at least one of the parameters. Required Permissions: To use
-- this action, an IAM user must have a Show, Deploy, or Manage permissions
-- level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
module Network.AWS.OpsWorks.DescribeLoadBasedAutoScaling where

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

data DescribeLoadBasedAutoScaling = DescribeLoadBasedAutoScaling
    { dlbasrLayerIds :: [Text]
      -- ^ An array of layer IDs.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeLoadBasedAutoScaling

instance AWSRequest DescribeLoadBasedAutoScaling where
    type Er DescribeLoadBasedAutoScaling = OpsWorksError
    type Rs DescribeLoadBasedAutoScaling = DescribeLoadBasedAutoScalingResponse
    request  = getJSON service
    response = responseJSON

data DescribeLoadBasedAutoScalingResponse = DescribeLoadBasedAutoScalingResponse
    { dlbasrrsLoadBasedAutoScalingConfigurations :: [LoadBasedAutoScalingConfiguration]
      -- ^ An array of LoadBasedAutoScalingConfiguration objects that describe each
      -- layer's configuration.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeLoadBasedAutoScalingResponse