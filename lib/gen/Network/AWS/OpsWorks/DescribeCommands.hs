{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DescribeCommands
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the results of specified commands. You must specify at least one
-- of the parameters. Required Permissions: To use this action, an IAM user
-- must have a Show, Deploy, or Manage permissions level for the stack, or an
-- attached policy that explicitly grants permissions. For more information on
-- user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.DescribeCommands where

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
describeCommands :: DescribeCommands
describeCommands = DescribeCommands
    { dcrCommandIds = []
    , dcrDeploymentId = Nothing
    , dcrInstanceId = Nothing
    }

data DescribeCommands = DescribeCommands
    { dcrCommandIds :: [Text]
      -- ^ An array of command IDs. If you include this parameter, DescribeCommands
      -- returns a description of the specified commands. Otherwise, it returns a
      -- description of every command.
    , dcrDeploymentId :: Maybe Text
      -- ^ The deployment ID. If you include this parameter, DescribeCommands returns
      -- a description of the commands associated with the specified deployment.
    , dcrInstanceId :: Maybe Text
      -- ^ The instance ID. If you include this parameter, DescribeCommands returns a
      -- description of the commands associated with the specified instance.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeCommands

instance AWSRequest DescribeCommands where
    type Er DescribeCommands = OpsWorksError
    type Rs DescribeCommands = DescribeCommandsResponse
    request  = getJSON service
    response = responseJSON

data DescribeCommandsResponse = DescribeCommandsResponse
    { dcrrsCommands :: [Command]
      -- ^ An array of Command objects that describe each of the specified commands.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeCommandsResponse
