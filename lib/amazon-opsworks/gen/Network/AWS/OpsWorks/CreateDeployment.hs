{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.CreateDeployment
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deploys a stack or app. App deployment generates a deploy event, which runs
-- the associated recipes and passes them a JSON stack configuration object
-- that includes information about the app. Stack deployment runs the deploy
-- recipes but does not raise an event. For more information, see Deploying
-- Apps and Run Stack Commands. Required Permissions: To use this action, an
-- IAM user must have a Deploy or Manage permissions level for the stack, or
-- an attached policy that explicitly grants permissions. For more information
-- on user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.CreateDeployment where

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
createDeployment :: DeploymentCommand
                 -> Text
                 -> CreateDeployment
createDeployment p1 p2 = CreateDeployment
    { cdrCommand = p1
    , cdrStackId = p2
    , cdrAppId = Nothing
    , cdrComment = Nothing
    , cdrCustomJson = Nothing
    , cdrInstanceIds = []
    }

data CreateDeployment = CreateDeployment
    { cdrAppId :: Maybe Text
      -- ^ The app ID. This parameter is required for app deployments, but not for
      -- other deployment commands.
    , cdrCommand :: DeploymentCommand
      -- ^ A DeploymentCommand object that specifies the deployment command and any
      -- associated arguments.
    , cdrComment :: Maybe Text
      -- ^ A user-defined comment.
    , cdrCustomJson :: Maybe Text
      -- ^ A string that contains user-defined, custom JSON. It is used to override
      -- the corresponding default stack configuration JSON values. The string
      -- should be in the following format and must escape characters such as '"'.:
      -- "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more information on
      -- custom JSON, see Use Custom JSON to Modify the Stack Configuration JSON.
    , cdrInstanceIds :: [Text]
      -- ^ The instance IDs for the deployment targets.
    , cdrStackId :: !Text
      -- ^ The stack ID.
    } deriving (Eq, Show, Generic)

instance ToJSON CreateDeployment where
    toJSON = genericToJSON jsonOptions

instance AWSRequest CreateDeployment where
    type Er CreateDeployment = OpsWorksError
    type Rs CreateDeployment = CreateDeploymentResponse
    request  = getJSON service
    response = responseJSON

data CreateDeploymentResponse = CreateDeploymentResponse
    { cdrrsDeploymentId :: Maybe Text
      -- ^ The deployment ID, which can be used with other requests to identify the
      -- deployment.
    } deriving (Eq, Show, Generic)

instance FromJSON CreateDeploymentResponse where
    fromJSON = genericFromJSON jsonOptions

