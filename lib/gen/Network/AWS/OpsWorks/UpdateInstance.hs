{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.UpdateInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates a specified instance. Required Permissions: To use this action, an
-- IAM user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.UpdateInstance where

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
updateInstance :: Text
               -> UpdateInstance
updateInstance p1 = undefined $ UpdateInstance
    { uirInstanceId = p1
    , uirAmiId = Nothing
    , uirArchitecture = Nothing
    , uirAutoScalingType = Nothing
    , uirHostname = Nothing
    , uirInstallUpdatesOnBoot = Nothing
    , uirInstanceType = Nothing
    , uirLayerIds = []
    , uirOs = Nothing
    , uirSshKeyName = Nothing
    }

data UpdateInstance = UpdateInstance
    { uirAmiId :: Maybe Text
      -- ^ A custom AMI ID to be used to create the instance. The AMI should be based
      -- on one of the standard AWS OpsWorks APIs: Amazon Linux or Ubuntu 12.04 LTS.
      -- For more information, see Instances.
    , uirArchitecture :: Maybe Architecture
      -- ^ The instance architecture. Instance types do not necessarily support both
      -- architectures. For a list of the architectures that are supported by the
      -- different instance types, see Instance Families and Types.
    , uirAutoScalingType :: Maybe AutoScalingType
      -- ^ The instance's auto scaling type, which has three possible values:
      -- AlwaysRunning: A 24/7 instance, which is not affected by auto scaling.
      -- TimeBasedAutoScaling: A time-based auto scaling instance, which is started
      -- and stopped based on a specified schedule. LoadBasedAutoScaling: A
      -- load-based auto scaling instance, which is started and stopped based on
      -- load metrics.
    , uirHostname :: Maybe Text
      -- ^ The instance host name.
    , uirInstallUpdatesOnBoot :: Maybe Bool
      -- ^ Whether to install operating system and package updates when the instance
      -- boots. The default value is true. To control when updates are installed,
      -- set this value to false. You must then update your instances manually by
      -- using CreateDeployment to run the update_dependencies stack command or
      -- manually running yum (Amazon Linux) or apt-get (Ubuntu) on the instances.
      -- We strongly recommend using the default value of true, to ensure that your
      -- instances have the latest security updates.
    , uirInstanceId :: !Text
      -- ^ The instance ID.
    , uirInstanceType :: Maybe Text
      -- ^ The instance type. AWS OpsWorks supports all instance types except Cluster
      -- Compute, Cluster GPU, and High Memory Cluster. For more information, see
      -- Instance Families and Types. The parameter values that you use to specify
      -- the various types are in the API Name column of the Available Instance
      -- Types table.
    , uirLayerIds :: [Text]
      -- ^ The instance's layer IDs.
    , uirOs :: Maybe Text
      -- ^ The instance operating system, which must be set to one of the following.
      -- Standard operating systems: Amazon Linux or Ubuntu 12.04 LTS Custom AMIs:
      -- Custom The default option is Amazon Linux. If you set this parameter to
      -- Custom, you must use the CreateInstance action's AmiId parameter to specify
      -- the custom AMI that you want to use. For more information on the standard
      -- operating systems, see Operating SystemsFor more information on how to use
      -- custom AMIs with OpsWorks, see Using Custom AMIs.
    , uirSshKeyName :: Maybe Text
      -- ^ The instance SSH key name.
    } deriving (Eq, Show, Generic)

instance ToJSON UpdateInstance

instance AWSRequest UpdateInstance where
    type Er UpdateInstance = OpsWorksError
    type Rs UpdateInstance = UpdateInstanceResponse
    request  = getJSON service
    response = responseJSON

data UpdateInstanceResponse = UpdateInstanceResponse
    deriving (Eq, Show, Generic)

instance FromJSON UpdateInstanceResponse
