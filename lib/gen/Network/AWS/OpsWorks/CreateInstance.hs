{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.CreateInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an instance in a specified stack. For more information, see Adding
-- an Instance to a Layer. Required Permissions: To use this action, an IAM
-- user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.CreateInstance where

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
createInstance :: Text
               -> [Text]
               -> Text
               -> CreateInstance
createInstance p1 p2 p3 = undefined $ CreateInstance
    { cirInstanceType = p1
    , cirLayerIds = p2
    , cirStackId = p3
    , cirAmiId = Nothing
    , cirArchitecture = Nothing
    , cirAutoScalingType = Nothing
    , cirAvailabilityZone = Nothing
    , cirHostname = Nothing
    , cirInstallUpdatesOnBoot = Nothing
    , cirOs = Nothing
    , cirRootDeviceType = Nothing
    , cirSshKeyName = Nothing
    , cirSubnetId = Nothing
    }

data CreateInstance = CreateInstance
    { cirAmiId :: Maybe Text
      -- ^ A custom AMI ID to be used to create the instance. The AMI should be based
      -- on one of the standard AWS OpsWorks APIs: Amazon Linux or Ubuntu 12.04 LTS.
      -- For more information, see Instances.
    , cirArchitecture :: Maybe Architecture
      -- ^ The instance architecture. Instance types do not necessarily support both
      -- architectures. For a list of the architectures that are supported by the
      -- different instance types, see Instance Families and Types.
    , cirAutoScalingType :: Maybe AutoScalingType
      -- ^ The instance auto scaling type, which has three possible values:
      -- AlwaysRunning: A 24/7 instance, which is not affected by auto scaling.
      -- TimeBasedAutoScaling: A time-based auto scaling instance, which is started
      -- and stopped based on a specified schedule. To specify the schedule, call
      -- SetTimeBasedAutoScaling. LoadBasedAutoScaling: A load-based auto scaling
      -- instance, which is started and stopped based on load metrics. To use
      -- load-based auto scaling, you must enable it for the instance layer and
      -- configure the thresholds by calling SetLoadBasedAutoScaling.
    , cirAvailabilityZone :: Maybe Text
      -- ^ The instance Availability Zone. For more information, see Regions and
      -- Endpoints.
    , cirHostname :: Maybe Text
      -- ^ The instance host name.
    , cirInstallUpdatesOnBoot :: Maybe Bool
      -- ^ Whether to install operating system and package updates when the instance
      -- boots. The default value is true. To control when updates are installed,
      -- set this value to false. You must then update your instances manually by
      -- using CreateDeployment to run the update_dependencies stack command or
      -- manually running yum (Amazon Linux) or apt-get (Ubuntu) on the instances.
      -- We strongly recommend using the default value of true, to ensure that your
      -- instances have the latest security updates.
    , cirInstanceType :: !Text
      -- ^ The instance type. AWS OpsWorks supports all instance types except Cluster
      -- Compute, Cluster GPU, and High Memory Cluster. For more information, see
      -- Instance Families and Types. The parameter values that you use to specify
      -- the various types are in the API Name column of the Available Instance
      -- Types table.
    , cirLayerIds :: [Text]
      -- ^ An array that contains the instance layer IDs.
    , cirOs :: Maybe Text
      -- ^ The instance operating system, which must be set to one of the following.
      -- Standard operating systems: Amazon Linux or Ubuntu 12.04 LTS Custom AMIs:
      -- Custom The default option is Amazon Linux. If you set this parameter to
      -- Custom, you must use the CreateInstance action's AmiId parameter to specify
      -- the custom AMI that you want to use. For more information on the standard
      -- operating systems, see Operating SystemsFor more information on how to use
      -- custom AMIs with OpsWorks, see Using Custom AMIs.
    , cirRootDeviceType :: Maybe RootDeviceType
      -- ^ The instance root device type. For more information, see Storage for the
      -- Root Device.
    , cirSshKeyName :: Maybe Text
      -- ^ The instance SSH key name.
    , cirStackId :: !Text
      -- ^ The stack ID.
    , cirSubnetId :: Maybe Text
      -- ^ The ID of the instance's subnet. If the stack is running in a VPC, you can
      -- use this parameter to override the stack's default subnet ID value and
      -- direct AWS OpsWorks to launch the instance in a different subnet.
    } deriving (Eq, Show, Generic)

instance ToJSON CreateInstance

instance AWSRequest CreateInstance where
    type Er CreateInstance = OpsWorksError
    type Rs CreateInstance = CreateInstanceResponse
    request  = getJSON service
    response = responseJSON

data CreateInstanceResponse = CreateInstanceResponse
    { cirrsInstanceId :: Maybe Text
      -- ^ The instance ID.
    } deriving (Eq, Show, Generic)

instance FromJSON CreateInstanceResponse
