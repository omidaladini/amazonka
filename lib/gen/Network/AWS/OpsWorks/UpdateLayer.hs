{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.UpdateLayer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates a specified layer. Required Permissions: To use this action, an IAM
-- user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.UpdateLayer where

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

-- | Convenience method utilising default fields where applicable.
updateLayer :: Text
            -> AWS (Either OpsWorksError UpdateLayerResponse)
updateLayer p1 = undefined $ UpdateLayer
    { ulrLayerId = p1
    , ulrAttributes = Map.empty
    , ulrAutoAssignElasticIps = Nothing
    , ulrAutoAssignPublicIps = Nothing
    , ulrCustomInstanceProfileArn = Nothing
    , ulrCustomRecipes = Nothing
    , ulrCustomSecurityGroupIds = []
    , ulrEnableAutoHealing = Nothing
    , ulrInstallUpdatesOnBoot = Nothing
    , ulrName = Nothing
    , ulrPackages = []
    , ulrShortname = Nothing
    , ulrVolumeConfigurations = []
    }

data UpdateLayer = UpdateLayer
    { ulrAttributes :: HashMap LayerAttributesKeys Text
      -- ^ One or more user-defined key/value pairs to be added to the stack
      -- attributes bag.
    , ulrAutoAssignElasticIps :: Maybe Bool
      -- ^ Whether to automatically assign an Elastic IP address to the layer's
      -- instances. For more information, see How to Edit a Layer.
    , ulrAutoAssignPublicIps :: Maybe Bool
      -- ^ For stacks that are running in a VPC, whether to automatically assign a
      -- public IP address to the layer's instances. For more information, see How
      -- to Edit a Layer.
    , ulrCustomInstanceProfileArn :: Maybe Text
      -- ^ The ARN of an IAM profile to be used for all of the layer's EC2 instances.
      -- For more information about IAM ARNs, see Using Identifiers.
    , ulrCustomRecipes :: Maybe Recipes
      -- ^ A LayerCustomRecipes object that specifies the layer's custom recipes.
    , ulrCustomSecurityGroupIds :: [Text]
      -- ^ An array containing the layer's custom security group IDs.
    , ulrEnableAutoHealing :: Maybe Bool
      -- ^ Whether to disable auto healing for the layer.
    , ulrInstallUpdatesOnBoot :: Maybe Bool
      -- ^ Whether to install operating system and package updates when the instance
      -- boots. The default value is true. To control when updates are installed,
      -- set this value to false. You must then update your instances manually by
      -- using CreateDeployment to run the update_dependencies stack command or
      -- manually running yum (Amazon Linux) or apt-get (Ubuntu) on the instances.
      -- We strongly recommend using the default value of true, to ensure that your
      -- instances have the latest security updates.
    , ulrLayerId :: !Text
      -- ^ The layer ID.
    , ulrName :: Maybe Text
      -- ^ The layer name, which is used by the console.
    , ulrPackages :: [Text]
      -- ^ An array of Package objects that describe the layer's packages.
    , ulrShortname :: Maybe Text
      -- ^ The layer short name, which is used internally by AWS OpsWorksand by Chef.
      -- The short name is also used as the name for the directory where your app
      -- files are installed. It can have a maximum of 200 characters and must be in
      -- the following format: /\A[a-z0-9\-\_\.]+\Z/.
    , ulrVolumeConfigurations :: [VolumeConfiguration]
      -- ^ A VolumeConfigurations object that describes the layer's Amazon EBS
      -- volumes.
    } deriving (Eq, Show, Generic)

instance ToJSON UpdateLayer

instance AWSRequest UpdateLayer where
    type Er UpdateLayer = OpsWorksError
    type Rs UpdateLayer = UpdateLayerResponse
    request  = getJSON service
    response = responseJSON

data UpdateLayerResponse = UpdateLayerResponse
    deriving (Eq, Show, Generic)

instance FromJSON UpdateLayerResponse
