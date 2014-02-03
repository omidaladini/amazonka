{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.CreateLayer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a layer. For more information, see How to Create a Layer. You
-- should use CreateLayer for noncustom layer types such as PHP App Server
-- only if the stack does not have an existing layer of that type. A stack can
-- have at most one instance of each noncustom layer; if you attempt to create
-- a second instance, CreateLayer fails. A stack can have an arbitrary number
-- of custom layers, so you can call CreateLayer as many times as you like for
-- that layer type. Required Permissions: To use this action, an IAM user must
-- have a Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see Managing User Permissions.
module Network.AWS.OpsWorks.CreateLayer where

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
createLayer :: Text
            -> Text
            -> Text
            -> LayerType
            -> CreateLayer
createLayer p1 p2 p3 p4 = CreateLayer
    { clrName = p1
    , clrShortname = p2
    , clrStackId = p3
    , clrType = p4
    , clrAttributes = Map.empty
    , clrAutoAssignElasticIps = Nothing
    , clrAutoAssignPublicIps = Nothing
    , clrCustomInstanceProfileArn = Nothing
    , clrCustomRecipes = Nothing
    , clrCustomSecurityGroupIds = []
    , clrEnableAutoHealing = Nothing
    , clrInstallUpdatesOnBoot = Nothing
    , clrPackages = []
    , clrVolumeConfigurations = []
    }

data CreateLayer = CreateLayer
    { clrAttributes :: HashMap LayerAttributesKeys Text
      -- ^ One or more user-defined key/value pairs to be added to the stack
      -- attributes bag.
    , clrAutoAssignElasticIps :: Maybe Bool
      -- ^ Whether to automatically assign an Elastic IP address to the layer's
      -- instances. For more information, see How to Edit a Layer.
    , clrAutoAssignPublicIps :: Maybe Bool
      -- ^ For stacks that are running in a VPC, whether to automatically assign a
      -- public IP address to the layer's instances. For more information, see How
      -- to Edit a Layer.
    , clrCustomInstanceProfileArn :: Maybe Text
      -- ^ The ARN of an IAM profile that to be used for the layer's EC2 instances.
      -- For more information about IAM ARNs, see Using Identifiers.
    , clrCustomRecipes :: Maybe Recipes
      -- ^ A LayerCustomRecipes object that specifies the layer custom recipes.
    , clrCustomSecurityGroupIds :: [Text]
      -- ^ An array containing the layer custom security group IDs.
    , clrEnableAutoHealing :: Maybe Bool
      -- ^ Whether to disable auto healing for the layer.
    , clrInstallUpdatesOnBoot :: Maybe Bool
      -- ^ Whether to install operating system and package updates when the instance
      -- boots. The default value is true. To control when updates are installed,
      -- set this value to false. You must then update your instances manually by
      -- using CreateDeployment to run the update_dependencies stack command or
      -- manually running yum (Amazon Linux) or apt-get (Ubuntu) on the instances.
      -- We strongly recommend using the default value of true, to ensure that your
      -- instances have the latest security updates.
    , clrName :: !Text
      -- ^ The layer name, which is used by the console.
    , clrPackages :: [Text]
      -- ^ An array of Package objects that describe the layer packages.
    , clrShortname :: !Text
      -- ^ The layer short name, which is used internally by AWS OpsWorks and by Chef
      -- recipes. The short name is also used as the name for the directory where
      -- your app files are installed. It can have a maximum of 200 characters,
      -- which are limited to the alphanumeric characters, '-', '_', and '.'.
    , clrStackId :: !Text
      -- ^ The layer stack ID.
    , clrType :: !LayerType
      -- ^ The layer type. A stack cannot have more than one layer of the same type.
      -- This parameter must be set to one of the following: lb: An HAProxy layer
      -- web: A Static Web Server layer rails-app: A Rails App Server layer php-app:
      -- A PHP App Server layer nodejs-app: A Node.js App Server layer memcached: A
      -- Memcached layer db-master: A MySQL layer monitoring-master: A Ganglia layer
      -- custom: A custom layer.
    , clrVolumeConfigurations :: [VolumeConfiguration]
      -- ^ A VolumeConfigurations object that describes the layer Amazon EBS volumes.
    } deriving (Eq, Show, Generic)

instance ToJSON CreateLayer where
    toJSON = genericToJSON jsonOptions

instance AWSRequest CreateLayer where
    type Er CreateLayer = OpsWorksError
    type Rs CreateLayer = CreateLayerResponse
    request  = getJSON service
    response = responseJSON

data CreateLayerResponse = CreateLayerResponse
    { clrrsLayerId :: Maybe Text
      -- ^ The layer ID.
    } deriving (Eq, Show, Generic)

instance FromJSON CreateLayerResponse where
    fromJSON = genericFromJSON jsonOptions

