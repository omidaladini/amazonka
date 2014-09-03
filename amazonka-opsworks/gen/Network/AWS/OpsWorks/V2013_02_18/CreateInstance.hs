{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.CreateInstance
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
module Network.AWS.OpsWorks.V2013_02_18.CreateInstance
    (
    -- * Request
      CreateInstance
    -- ** Request constructor
    , createInstance
    -- ** Request lenses
    , cirStackId
    , cirInstanceType
    , cirLayerIds
    , cirArchitecture
    , cirAutoScalingType
    , cirInstallUpdatesOnBoot
    , cirEbsOptimized
    , cirRootDeviceType
    , cirHostname
    , cirOs
    , cirAmiId
    , cirSshKeyName
    , cirAvailabilityZone
    , cirVirtualizationType
    , cirSubnetId

    -- * Response
    , CreateInstanceResponse
    -- ** Response lenses
    , cisInstanceId
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'CreateInstance' request.
createInstance :: Text -- ^ 'cirStackId'
               -> Text -- ^ 'cirInstanceType'
               -> [Text] -- ^ 'cirLayerIds'
               -> CreateInstance
createInstance p1 p2 p3 = CreateInstance
    { _cirStackId = p1
    , _cirInstanceType = p2
    , _cirLayerIds = p3
    , _cirArchitecture = Nothing
    , _cirAutoScalingType = Nothing
    , _cirInstallUpdatesOnBoot = Nothing
    , _cirEbsOptimized = Nothing
    , _cirRootDeviceType = Nothing
    , _cirHostname = Nothing
    , _cirOs = Nothing
    , _cirAmiId = Nothing
    , _cirSshKeyName = Nothing
    , _cirAvailabilityZone = Nothing
    , _cirVirtualizationType = Nothing
    , _cirSubnetId = Nothing
    }

data CreateInstance = CreateInstance
    { _cirStackId :: Text
      -- ^ The stack ID.
    , _cirInstanceType :: Text
      -- ^ The instance type. AWS OpsWorks supports all instance types
      -- except Cluster Compute, Cluster GPU, and High Memory Cluster. For
      -- more information, see Instance Families and Types. The parameter
      -- values that you use to specify the various types are in the API
      -- Name column of the Available Instance Types table.
    , _cirLayerIds :: [Text]
      -- ^ An array that contains the instance layer IDs.
    , _cirArchitecture :: Maybe Architecture
      -- ^ The instance architecture. The default option is x86_64. Instance
      -- types do not necessarily support both architectures. For a list
      -- of the architectures that are supported by the different instance
      -- types, see Instance Families and Types.
    , _cirAutoScalingType :: Maybe AutoScalingType
      -- ^ The instance auto scaling type, which has three possible values:
      -- AlwaysRunning: A 24/7 instance, which is not affected by auto
      -- scaling. TimeBasedAutoScaling: A time-based auto scaling
      -- instance, which is started and stopped based on a specified
      -- schedule. To specify the schedule, call SetTimeBasedAutoScaling.
      -- LoadBasedAutoScaling: A load-based auto scaling instance, which
      -- is started and stopped based on load metrics. To use load-based
      -- auto scaling, you must enable it for the instance layer and
      -- configure the thresholds by calling SetLoadBasedAutoScaling.
    , _cirInstallUpdatesOnBoot :: Maybe Bool
      -- ^ Whether to install operating system and package updates when the
      -- instance boots. The default value is true. To control when
      -- updates are installed, set this value to false. You must then
      -- update your instances manually by using CreateDeployment to run
      -- the update_dependencies stack command or manually running yum
      -- (Amazon Linux) or apt-get (Ubuntu) on the instances. We strongly
      -- recommend using the default value of true to ensure that your
      -- instances have the latest security updates.
    , _cirEbsOptimized :: Maybe Bool
      -- ^ Whether to create an Amazon EBS-optimized instance.
    , _cirRootDeviceType :: Maybe RootDeviceType
      -- ^ The instance root device type. For more information, see Storage
      -- for the Root Device.
    , _cirHostname :: Maybe Text
      -- ^ The instance host name.
    , _cirOs :: Maybe Text
      -- ^ The instance operating system, which must be set to one of the
      -- following. Standard operating systems: Amazon Linux or Ubuntu
      -- 12.04 LTS Custom AMIs: Custom The default option is Amazon Linux.
      -- If you set this parameter to Custom, you must use the
      -- CreateInstance action's AmiId parameter to specify the custom AMI
      -- that you want to use. For more information on the standard
      -- operating systems, see Operating SystemsFor more information on
      -- how to use custom AMIs with OpsWorks, see Using Custom AMIs.
    , _cirAmiId :: Maybe Text
      -- ^ A custom AMI ID to be used to create the instance. The AMI should
      -- be based on one of the standard AWS OpsWorks APIs: Amazon Linux
      -- or Ubuntu 12.04 LTS. For more information, see Instances.
    , _cirSshKeyName :: Maybe Text
      -- ^ The instance SSH key name.
    , _cirAvailabilityZone :: Maybe Text
      -- ^ The instance Availability Zone. For more information, see Regions
      -- and Endpoints.
    , _cirVirtualizationType :: Maybe Text
      -- ^ The instance's virtualization type, paravirtual or hvm.
    , _cirSubnetId :: Maybe Text
      -- ^ The ID of the instance's subnet. If the stack is running in a
      -- VPC, you can use this parameter to override the stack's default
      -- subnet ID value and direct AWS OpsWorks to launch the instance in
      -- a different subnet.
    } deriving (Show, Generic)

-- | The stack ID.
cirStackId
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateInstance
    -> f CreateInstance
cirStackId f x =
    (\y -> x { _cirStackId = y })
       <$> f (_cirStackId x)
{-# INLINE cirStackId #-}

-- | The instance type. AWS OpsWorks supports all instance types except Cluster
-- Compute, Cluster GPU, and High Memory Cluster. For more information, see
-- Instance Families and Types. The parameter values that you use to specify
-- the various types are in the API Name column of the Available Instance
-- Types table.
cirInstanceType
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateInstance
    -> f CreateInstance
cirInstanceType f x =
    (\y -> x { _cirInstanceType = y })
       <$> f (_cirInstanceType x)
{-# INLINE cirInstanceType #-}

-- | An array that contains the instance layer IDs.
cirLayerIds
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> CreateInstance
    -> f CreateInstance
cirLayerIds f x =
    (\y -> x { _cirLayerIds = y })
       <$> f (_cirLayerIds x)
{-# INLINE cirLayerIds #-}

-- | The instance architecture. The default option is x86_64. Instance types do
-- not necessarily support both architectures. For a list of the architectures
-- that are supported by the different instance types, see Instance Families
-- and Types.
cirArchitecture
    :: Functor f
    => (Maybe Architecture
    -> f (Maybe Architecture))
    -> CreateInstance
    -> f CreateInstance
cirArchitecture f x =
    (\y -> x { _cirArchitecture = y })
       <$> f (_cirArchitecture x)
{-# INLINE cirArchitecture #-}

-- | The instance auto scaling type, which has three possible values:
-- AlwaysRunning: A 24/7 instance, which is not affected by auto scaling.
-- TimeBasedAutoScaling: A time-based auto scaling instance, which is started
-- and stopped based on a specified schedule. To specify the schedule, call
-- SetTimeBasedAutoScaling. LoadBasedAutoScaling: A load-based auto scaling
-- instance, which is started and stopped based on load metrics. To use
-- load-based auto scaling, you must enable it for the instance layer and
-- configure the thresholds by calling SetLoadBasedAutoScaling.
cirAutoScalingType
    :: Functor f
    => (Maybe AutoScalingType
    -> f (Maybe AutoScalingType))
    -> CreateInstance
    -> f CreateInstance
cirAutoScalingType f x =
    (\y -> x { _cirAutoScalingType = y })
       <$> f (_cirAutoScalingType x)
{-# INLINE cirAutoScalingType #-}

-- | Whether to install operating system and package updates when the instance
-- boots. The default value is true. To control when updates are installed,
-- set this value to false. You must then update your instances manually by
-- using CreateDeployment to run the update_dependencies stack command or
-- manually running yum (Amazon Linux) or apt-get (Ubuntu) on the instances.
-- We strongly recommend using the default value of true to ensure that your
-- instances have the latest security updates.
cirInstallUpdatesOnBoot
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> CreateInstance
    -> f CreateInstance
cirInstallUpdatesOnBoot f x =
    (\y -> x { _cirInstallUpdatesOnBoot = y })
       <$> f (_cirInstallUpdatesOnBoot x)
{-# INLINE cirInstallUpdatesOnBoot #-}

-- | Whether to create an Amazon EBS-optimized instance.
cirEbsOptimized
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> CreateInstance
    -> f CreateInstance
cirEbsOptimized f x =
    (\y -> x { _cirEbsOptimized = y })
       <$> f (_cirEbsOptimized x)
{-# INLINE cirEbsOptimized #-}

-- | The instance root device type. For more information, see Storage for the
-- Root Device.
cirRootDeviceType
    :: Functor f
    => (Maybe RootDeviceType
    -> f (Maybe RootDeviceType))
    -> CreateInstance
    -> f CreateInstance
cirRootDeviceType f x =
    (\y -> x { _cirRootDeviceType = y })
       <$> f (_cirRootDeviceType x)
{-# INLINE cirRootDeviceType #-}

-- | The instance host name.
cirHostname
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateInstance
    -> f CreateInstance
cirHostname f x =
    (\y -> x { _cirHostname = y })
       <$> f (_cirHostname x)
{-# INLINE cirHostname #-}

-- | The instance operating system, which must be set to one of the following.
-- Standard operating systems: Amazon Linux or Ubuntu 12.04 LTS Custom AMIs:
-- Custom The default option is Amazon Linux. If you set this parameter to
-- Custom, you must use the CreateInstance action's AmiId parameter to specify
-- the custom AMI that you want to use. For more information on the standard
-- operating systems, see Operating SystemsFor more information on how to use
-- custom AMIs with OpsWorks, see Using Custom AMIs.
cirOs
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateInstance
    -> f CreateInstance
cirOs f x =
    (\y -> x { _cirOs = y })
       <$> f (_cirOs x)
{-# INLINE cirOs #-}

-- | A custom AMI ID to be used to create the instance. The AMI should be based
-- on one of the standard AWS OpsWorks APIs: Amazon Linux or Ubuntu 12.04 LTS.
-- For more information, see Instances.
cirAmiId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateInstance
    -> f CreateInstance
cirAmiId f x =
    (\y -> x { _cirAmiId = y })
       <$> f (_cirAmiId x)
{-# INLINE cirAmiId #-}

-- | The instance SSH key name.
cirSshKeyName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateInstance
    -> f CreateInstance
cirSshKeyName f x =
    (\y -> x { _cirSshKeyName = y })
       <$> f (_cirSshKeyName x)
{-# INLINE cirSshKeyName #-}

-- | The instance Availability Zone. For more information, see Regions and
-- Endpoints.
cirAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateInstance
    -> f CreateInstance
cirAvailabilityZone f x =
    (\y -> x { _cirAvailabilityZone = y })
       <$> f (_cirAvailabilityZone x)
{-# INLINE cirAvailabilityZone #-}

-- | The instance's virtualization type, paravirtual or hvm.
cirVirtualizationType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateInstance
    -> f CreateInstance
cirVirtualizationType f x =
    (\y -> x { _cirVirtualizationType = y })
       <$> f (_cirVirtualizationType x)
{-# INLINE cirVirtualizationType #-}

-- | The ID of the instance's subnet. If the stack is running in a VPC, you can
-- use this parameter to override the stack's default subnet ID value and
-- direct AWS OpsWorks to launch the instance in a different subnet.
cirSubnetId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateInstance
    -> f CreateInstance
cirSubnetId f x =
    (\y -> x { _cirSubnetId = y })
       <$> f (_cirSubnetId x)
{-# INLINE cirSubnetId #-}

instance ToPath CreateInstance

instance ToQuery CreateInstance

instance ToHeaders CreateInstance

instance ToJSON CreateInstance

data CreateInstanceResponse = CreateInstanceResponse
    { _cisInstanceId :: Maybe Text
      -- ^ The instance ID.
    } deriving (Show, Generic)

-- | The instance ID.
cisInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateInstanceResponse
    -> f CreateInstanceResponse
cisInstanceId f x =
    (\y -> x { _cisInstanceId = y })
       <$> f (_cisInstanceId x)
{-# INLINE cisInstanceId #-}

instance FromJSON CreateInstanceResponse

instance AWSRequest CreateInstance where
    type Sv CreateInstance = OpsWorks
    type Rs CreateInstance = CreateInstanceResponse

    request = get
    response _ = jsonResponse
