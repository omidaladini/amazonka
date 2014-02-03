{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DescribeInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Requests a description of a set of instances. You must specify at least one
-- of the parameters. Required Permissions: To use this action, an IAM user
-- must have a Show, Deploy, or Manage permissions level for the stack, or an
-- attached policy that explicitly grants permissions. For more information on
-- user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.DescribeInstances where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeInstances :: DescribeInstances
describeInstances = DescribeInstances
    { disInstanceIds = []
    , disLayerId = Nothing
    , disStackId = Nothing
    }

data DescribeInstances = DescribeInstances
    { disInstanceIds :: [Text]
      -- ^ An array of instance IDs to be described. If you use this parameter,
      -- DescribeInstances returns a description of the specified instances.
      -- Otherwise, it returns a description of every instance.
    , disLayerId :: Maybe Text
      -- ^ A layer ID. If you use this parameter, DescribeInstances returns
      -- descriptions of the instances associated with the specified layer.
    , disStackId :: Maybe Text
      -- ^ A stack ID. If you use this parameter, DescribeInstances returns
      -- descriptions of the instances associated with the specified stack.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeInstances where
    toJSON = genericToJSON jsonOptions

instance AWSRequest DescribeInstances where
    type Er DescribeInstances = OpsWorksError
    type Rs DescribeInstances = DescribeInstancesResponse
    request  = getJSON service
    response = responseJSON

data DescribeInstancesResponse = DescribeInstancesResponse
    { disrsInstances :: [Instance]
      -- ^ An array of Instance objects that describe the instances.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeInstancesResponse where
    fromJSON = genericFromJSON jsonOptions

