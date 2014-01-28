{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.AutoScaling.Types where

import Data.ByteString      (ByteString)
import Data.Hashable
import Data.Monoid
import Data.List.NonEmpty
import Data.Text            (Text)
import Data.HashMap.Strict  (HashMap)
import Data.Time
import GHC.Generics         (Generic)
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.AutoScaling.Service

-- | The tag applied to an Auto Scaling group.
data TagDescription = TagDescription
    { tdKey :: Maybe Text
      -- ^ The key of the tag.
    , tdPropagateAtLaunch :: Maybe Bool
      -- ^ Specifies whether the new tag will be applied to instances launched after
      -- the tag is created. The same behavior applies to updates: If you change a
      -- tag, the changed tag will be applied to all instances launched after you
      -- made the change.
    , tdResourceId :: Maybe Text
      -- ^ The name of the Auto Scaling group.
    , tdResourceType :: Maybe Text
      -- ^ The kind of resource to which the tag is applied. Currently, Auto Scaling
      -- supports the auto-scaling-group resource type.
    , tdValue :: Maybe Text
      -- ^ The value of the tag.
    } deriving (Eq, Show, Generic)

instance ToQuery TagDescription

instance FromXML TagDescription where
    fromXMLOptions = xmlOptions

instance ToXML TagDescription where
    toXMLOptions = xmlOptions

-- | The tag applied to an Auto Scaling group.
data Tag = Tag
    { tKey :: !Text
      -- ^ The key of the tag.
    , tPropagateAtLaunch :: Maybe Bool
      -- ^ Specifies whether the new tag will be applied to instances launched after
      -- the tag is created. The same behavior applies to updates: If you change a
      -- tag, the changed tag will be applied to all instances launched after you
      -- made the change.
    , tResourceId :: Maybe Text
      -- ^ The name of the Auto Scaling group.
    , tResourceType :: Maybe Text
      -- ^ The kind of resource to which the tag is applied. Currently, Auto Scaling
      -- supports the auto-scaling-group resource type.
    , tValue :: Maybe Text
      -- ^ The value of the tag.
    } deriving (Eq, Show, Generic)

instance ToQuery Tag

instance FromXML Tag where
    fromXMLOptions = xmlOptions

instance ToXML Tag where
    toXMLOptions = xmlOptions

-- | An Auto Scaling process that has been suspended. For more information, see
-- ProcessType.
data SuspendedProcess = SuspendedProcess
    { sqProcessName :: Maybe Text
      -- ^ The name of the suspended process.
    , sqSuspensionReason :: Maybe Text
      -- ^ The reason that the process was suspended.
    } deriving (Eq, Show, Generic)

instance ToQuery SuspendedProcess

instance FromXML SuspendedProcess where
    fromXMLOptions = xmlOptions

instance ToXML SuspendedProcess where
    toXMLOptions = xmlOptions

-- | This data type stores information about a scheduled update to an Auto
-- Scaling group.
data ScheduledUpdateGroupAction = ScheduledUpdateGroupAction
    { sugaAutoScalingGroupName :: Maybe Text
      -- ^ The name of the Auto Scaling group to be updated.
    , sugaDesiredCapacity :: Maybe Int
      -- ^ The number of instances you prefer to maintain in your Auto Scaling group.
    , sugaEndTime :: Maybe UTCTime
      -- ^ The time that the action is scheduled to end. This value can be up to one
      -- month in the future.
    , sugaMaxSize :: Maybe Int
      -- ^ The maximum size of the Auto Scaling group.
    , sugaMinSize :: Maybe Int
      -- ^ The minimum size of the Auto Scaling group.
    , sugaRecurrence :: Maybe Text
      -- ^ The regular schedule that an action occurs.
    , sugaScheduledActionARN :: Maybe ResourceName
      -- ^ The Amazon Resource Name (ARN) of this scheduled action.
    , sugaScheduledActionName :: Maybe Text
      -- ^ The name of this scheduled action.
    , sugaStartTime :: Maybe UTCTime
      -- ^ The time that the action is scheduled to begin. This value can be up to one
      -- month in the future. When StartTime and EndTime are specified with
      -- Recurrence, they form the boundaries of when the recurring action will
      -- start and stop.
    , sugaTime :: Maybe UTCTime
      -- ^ Time is deprecated. The time that the action is scheduled to begin. Time is
      -- an alias for StartTime.
    } deriving (Eq, Show, Generic)

instance ToQuery ScheduledUpdateGroupAction

instance FromXML ScheduledUpdateGroupAction where
    fromXMLOptions = xmlOptions

instance ToXML ScheduledUpdateGroupAction where
    toXMLOptions = xmlOptions

-- | The ScalingPolicy data type.
data ScalingPolicy = ScalingPolicy
    { spAdjustmentType :: Maybe Text
      -- ^ Specifies whether the ScalingAdjustment is an absolute number or a
      -- percentage of the current capacity. Valid values are ChangeInCapacity,
      -- ExactCapacity, and PercentChangeInCapacity.
    , spAlarms :: [Alarm]
      -- ^ A list of CloudWatch Alarms related to the policy.
    , spAutoScalingGroupName :: Maybe Text
      -- ^ The name of the Auto Scaling group associated with this scaling policy.
    , spCooldown :: Maybe Int
      -- ^ The amount of time, in seconds, after a scaling activity completes before
      -- any further trigger-related scaling activities can start.
    , spMinAdjustmentStep :: Maybe Int
      -- ^ Changes the DesiredCapacity of the Auto Scaling group by at least the
      -- specified number of instances.
    , spPolicyARN :: Maybe ResourceName
      -- ^ The Amazon Resource Name (ARN) of the policy.
    , spPolicyName :: Maybe Text
      -- ^ The name of the scaling policy.
    , spScalingAdjustment :: Maybe Int
      -- ^ The number associated with the specified adjustment type. A positive value
      -- adds to the current capacity and a negative value removes from the current
      -- capacity.
    } deriving (Eq, Show, Generic)

instance ToQuery ScalingPolicy

instance FromXML ScalingPolicy where
    fromXMLOptions = xmlOptions

instance ToXML ScalingPolicy where
    toXMLOptions = xmlOptions

-- | There are two primary Auto Scaling process types--Launch and Terminate. The
-- Launch process creates a new Amazon EC2 instance for an Auto Scaling group,
-- and the Terminate process removes an existing Amazon EC2 instance. The
-- remaining Auto Scaling process types relate to specific Auto Scaling
-- features: AddToLoadBalancer AlarmNotification AZRebalance HealthCheck
-- ReplaceUnhealthy ScheduledActions If you suspend Launch or Terminate, all
-- other process types are affected to varying degrees. The following
-- descriptions discuss how each process type is affected by a suspension of
-- Launch or Terminate. The AddToLoadBalancer process type adds instances to
-- the load balancer when the instances are launched. If you suspend this
-- process, Auto Scaling will launch the instances but will not add them to
-- the load balancer. If you resume the AddToLoadBalancer process, Auto
-- Scaling will also resume adding new instances to the load balancer when
-- they are launched. However, Auto Scaling will not add running instances
-- that were launched while the process was suspended; those instances must be
-- added manually using the the RegisterInstancesWithLoadBalancer call in the
-- Elastic Load Balancing API Reference. The AlarmNotification process type
-- accepts notifications from Amazon CloudWatch alarms that are associated
-- with the Auto Scaling group. If you suspend the AlarmNotification process
-- type, Auto Scaling will not automatically execute scaling policies that
-- would be triggered by alarms. Although the AlarmNotification process type
-- is not directly affected by a suspension of Launch or Terminate, alarm
-- notifications are often used to signal that a change in the size of the
-- Auto Scaling group is warranted. If you suspend Launch or Terminate, Auto
-- Scaling might not be able to implement the alarm's associated policy. The
-- AZRebalance process type seeks to maintain a balanced number of instances
-- across Availability Zones within a Region. If you remove an Availability
-- Zone from your Auto Scaling group or an Availability Zone otherwise becomes
-- unhealthy or unavailable, Auto Scaling launches new instances in an
-- unaffected Availability Zone before terminating the unhealthy or
-- unavailable instances. When the unhealthy Availability Zone returns to a
-- healthy state, Auto Scaling automatically redistributes the application
-- instances evenly across all of the designated Availability Zones. If you
-- call SuspendProcesses on the launch process type, the AZRebalance process
-- will neither launch new instances nor terminate existing instances. This is
-- because the AZRebalance process terminates existing instances only after
-- launching the replacement instances. If you call SuspendProcesses on the
-- terminate process type, the AZRebalance process can cause your Auto Scaling
-- group to grow up to ten percent larger than the maximum size. This is
-- because Auto Scaling allows groups to temporarily grow larger than the
-- maximum size during rebalancing activities. If Auto Scaling cannot
-- terminate instances, your Auto Scaling group could remain up to ten percent
-- larger than the maximum size until you resume the terminate process type.
-- The HealthCheck process type checks the health of the instances. Auto
-- Scaling marks an instance as unhealthy if Amazon EC2 or Elastic Load
-- Balancing informs Auto Scaling that the instance is unhealthy. The
-- HealthCheck process can override the health status of an instance that you
-- set with SetInstanceHealth. The ReplaceUnhealthy process type terminates
-- instances that are marked as unhealthy and subsequently creates new
-- instances to replace them. This process calls both of the primary process
-- types--first Terminate and then Launch. The HealthCheck process type works
-- in conjunction with the ReplaceUnhealthly process type to provide health
-- check functionality. If you suspend either Launch or Terminate, the
-- ReplaceUnhealthy process type will not function properly. The
-- ScheduledActions process type performs scheduled actions that you create
-- with PutScheduledUpdateGroupAction. Scheduled actions often involve
-- launching new instances or terminating existing instances. If you suspend
-- either Launch or Terminate, your scheduled actions might not function as
-- expected.
newtype ProcessType = ProcessType
    { puProcessName :: Text
      -- ^ The name of a process.
    } deriving (Eq, Show, Generic)

instance ToQuery ProcessType

instance FromXML ProcessType where
    fromXMLOptions = xmlOptions

instance ToXML ProcessType where
    toXMLOptions = xmlOptions

-- | The NotificationConfiguration data type.
data NotificationConfiguration = NotificationConfiguration
    { ncAutoScalingGroupName :: Maybe ResourceName
      -- ^ Specifies the Auto Scaling group name.
    , ncNotificationType :: Maybe Text
      -- ^ The types of events for an action to start.
    , ncTopicARN :: Maybe ResourceName
      -- ^ The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
      -- (SNS) topic.
    } deriving (Eq, Show, Generic)

instance ToQuery NotificationConfiguration

instance FromXML NotificationConfiguration where
    fromXMLOptions = xmlOptions

instance ToXML NotificationConfiguration where
    toXMLOptions = xmlOptions

-- | The MetricGranularityType data type.
newtype MetricGranularityType = MetricGranularityType
    { mgtGranularity :: Maybe Text
      -- ^ The granularity of a Metric.
    } deriving (Eq, Show, Generic)

instance ToQuery MetricGranularityType

instance FromXML MetricGranularityType where
    fromXMLOptions = xmlOptions

instance ToXML MetricGranularityType where
    toXMLOptions = xmlOptions

-- | The MetricCollectionType data type.
newtype MetricCollectionType = MetricCollectionType
    { mctMetric :: Maybe Text
      -- ^ FIXME: Type documentation for Text
    } deriving (Eq, Show, Generic)

instance ToQuery MetricCollectionType

instance FromXML MetricCollectionType where
    fromXMLOptions = xmlOptions

instance ToXML MetricCollectionType where
    toXMLOptions = xmlOptions

-- | The LaunchConfiguration data type.
data LaunchConfiguration = LaunchConfiguration
    { lcAssociatePublicIpAddress :: Maybe Bool
      -- ^ Specifies whether the instance is associated with a public IP address
      -- (true) or not (false).
    , lcBlockDeviceMappings :: [BlockDeviceMapping]
      -- ^ Specifies how block devices are exposed to the instance. Each mapping is
      -- made up of a virtualName and a deviceName.
    , lcCreatedTime :: !UTCTime
      -- ^ Provides the creation date and time for this launch configuration.
    , lcEbsOptimized :: Maybe Bool
      -- ^ Specifies whether the instance is optimized for EBS I/O (true) or not
      -- (false).
    , lcIamInstanceProfile :: Maybe Text
      -- ^ Provides the name or the Amazon Resource Name (ARN) of the instance profile
      -- associated with the IAM role for the instance. The instance profile
      -- contains the IAM role.
    , lcImageId :: !Text
      -- ^ Provides the unique ID of the Amazon Machine Image (AMI) that was assigned
      -- during registration.
    , lcInstanceMonitoring :: Maybe InstanceMonitoring
      -- ^ Controls whether instances in this group are launched with detailed
      -- monitoring or not.
    , lcInstanceType :: !Text
      -- ^ Specifies the instance type of the Amazon EC2 instance.
    , lcKernelId :: Maybe Text
      -- ^ Provides the ID of the kernel associated with the Amazon EC2 AMI.
    , lcKeyName :: Maybe Text
      -- ^ Provides the name of the Amazon EC2 key pair.
    , lcLaunchConfigurationARN :: Maybe ResourceName
      -- ^ The launch configuration's Amazon Resource Name (ARN).
    , lcLaunchConfigurationName :: !Text
      -- ^ Specifies the name of the launch configuration.
    , lcRamdiskId :: Maybe Text
      -- ^ Provides ID of the RAM disk associated with the Amazon EC2 AMI.
    , lcSecurityGroups :: [Text]
      -- ^ A description of the security groups to associate with the Amazon EC2
      -- instances.
    , lcSpotPrice :: Maybe Text
      -- ^ Specifies the price to bid when launching Spot Instances.
    , lcUserData :: Maybe Text
      -- ^ The user data available to the launched Amazon EC2 instances.
    } deriving (Eq, Show, Generic)

instance ToQuery LaunchConfiguration

instance FromXML LaunchConfiguration where
    fromXMLOptions = xmlOptions

instance ToXML LaunchConfiguration where
    toXMLOptions = xmlOptions

-- | Controls whether instances in this group are launched with detailed
-- monitoring or not.
newtype InstanceMonitoring = InstanceMonitoring
    { imEnabled :: Maybe Bool
      -- ^ If True, instance monitoring is enabled.
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceMonitoring

instance FromXML InstanceMonitoring where
    fromXMLOptions = xmlOptions

instance ToXML InstanceMonitoring where
    toXMLOptions = xmlOptions

-- | The Instance data type.
data Instance = Instance
    { iAvailabilityZone :: !Text
      -- ^ Availability Zones associated with this instance.
    , iHealthStatus :: !Text
      -- ^ The instance's health status.
    , iInstanceId :: !Text
      -- ^ Specifies the ID of the Amazon EC2 instance.
    , iLaunchConfigurationName :: !Text
      -- ^ The launch configuration associated with this instance.
    , iLifecycleState :: !LifecycleState
      -- ^ Contains a description of the current lifecycle state. The Quarantined
      -- lifecycle state is currently not used.
    } deriving (Eq, Show, Generic)

instance ToQuery Instance

instance FromXML Instance where
    fromXMLOptions = xmlOptions

instance ToXML Instance where
    toXMLOptions = xmlOptions

-- | The Filter data type.
data Filter = Filter
    { fName :: Maybe Text
      -- ^ The name of the filter. Valid Name values are: "auto-scaling-group", "key",
      -- "value", and "propagate-at-launch".
    , fValues :: [Text]
      -- ^ The value of the filter.
    } deriving (Eq, Show, Generic)

instance ToQuery Filter

instance FromXML Filter where
    fromXMLOptions = xmlOptions

instance ToXML Filter where
    toXMLOptions = xmlOptions

-- | The EnabledMetric data type.
data EnabledMetric = EnabledMetric
    { emGranularity :: Maybe Text
      -- ^ The granularity of the enabled metric.
    , emMetric :: Maybe Text
      -- ^ The name of the enabled metric.
    } deriving (Eq, Show, Generic)

instance ToQuery EnabledMetric

instance FromXML EnabledMetric where
    fromXMLOptions = xmlOptions

instance ToXML EnabledMetric where
    toXMLOptions = xmlOptions

-- | The Elastic Block Storage volume information.
data Ebs = Ebs
    { eDeleteOnTermination :: Maybe Bool
      -- ^ Indicates whether to delete the volume on instance termination. Default:
      -- true.
    , eIops :: Maybe Int
      -- ^ The number of I/O operations per second (IOPS) that the volume supports.
      -- The maximum ratio of IOPS to volume size is 30.0 Valid Values: Range is 100
      -- to 4000. Default: None.
    , eSnapshotId :: Maybe Text
      -- ^ The snapshot ID.
    , eVolumeSize :: Maybe Int
      -- ^ The volume size, in gigabytes. Valid values: If the volume type is io1, the
      -- minimum size of the volume is 10. Default: If you're creating the volume
      -- from a snapshot, and you don't specify a volume size, the default is the
      -- snapshot size. Required: Required when the volume type is io1.
    , eVolumeType :: Maybe Text
      -- ^ The volume type. Valid values: standard | io1 Default: standard.
    } deriving (Eq, Show, Generic)

instance ToQuery Ebs

instance FromXML Ebs where
    fromXMLOptions = xmlOptions

instance ToXML Ebs where
    toXMLOptions = xmlOptions

-- | The BlockDeviceMapping data type.
data BlockDeviceMapping = BlockDeviceMapping
    { bdmDeviceName :: !Text
      -- ^ The name of the device within Amazon EC2 (for example, /dev/sdh or xvdh).
    , bdmEbs :: Maybe Ebs
      -- ^ The Elastic Block Storage volume information.
    , bdmNoDevice :: Maybe Bool
      -- ^ Suppresses the device mapping. If NoDevice is set to true for the root
      -- device, the instance might fail the EC2 health check. Auto Scaling launches
      -- a replacement instance if the instance fails the health check.
    , bdmVirtualName :: Maybe Text
      -- ^ The virtual name associated with the device.
    } deriving (Eq, Show, Generic)

instance ToQuery BlockDeviceMapping

instance FromXML BlockDeviceMapping where
    fromXMLOptions = xmlOptions

instance ToXML BlockDeviceMapping where
    toXMLOptions = xmlOptions

-- | The AutoScalingInstanceDetails data type.
data AutoScalingInstanceDetails = AutoScalingInstanceDetails
    { asidAutoScalingGroupName :: !Text
      -- ^ The name of the Auto Scaling group associated with this instance.
    , asidAvailabilityZone :: !Text
      -- ^ The Availability Zone in which this instance resides.
    , asidHealthStatus :: !Text
      -- ^ The health status of this instance. "Healthy" means that the instance is
      -- healthy and should remain in service. "Unhealthy" means that the instance
      -- is unhealthy. Auto Scaling should terminate and replace it.
    , asidInstanceId :: !Text
      -- ^ The instance ID of the Amazon EC2 instance.
    , asidLaunchConfigurationName :: !Text
      -- ^ The launch configuration associated with this instance.
    , asidLifecycleState :: !Text
      -- ^ The life cycle state of this instance. for more information, see Instance
      -- Lifecycle State in the Auto Scaling Developer Guide.
    } deriving (Eq, Show, Generic)

instance ToQuery AutoScalingInstanceDetails

instance FromXML AutoScalingInstanceDetails where
    fromXMLOptions = xmlOptions

instance ToXML AutoScalingInstanceDetails where
    toXMLOptions = xmlOptions

-- | The AutoScalingGroup data type.
data AutoScalingGroup = AutoScalingGroup
    { asgAutoScalingGroupARN :: Maybe ResourceName
      -- ^ The Amazon Resource Name (ARN) of the Auto Scaling group.
    , asgAutoScalingGroupName :: !Text
      -- ^ Specifies the name of the group.
    , asgAvailabilityZones :: [Text]
      -- ^ Contains a list of Availability Zones for the group.
    , asgCreatedTime :: !UTCTime
      -- ^ Specifies the date and time the Auto Scaling group was created.
    , asgDefaultCooldown :: !Int
      -- ^ The number of seconds after a scaling activity completes before any further
      -- scaling activities can start.
    , asgDesiredCapacity :: !Int
      -- ^ Specifies the desired capacity for the Auto Scaling group.
    , asgEnabledMetrics :: [EnabledMetric]
      -- ^ A list of metrics enabled for this Auto Scaling group.
    , asgHealthCheckGracePeriod :: Maybe Int
      -- ^ The length of time that Auto Scaling waits before checking an instance's
      -- health status. The grace period begins when an instance comes into service.
    , asgHealthCheckType :: !Text
      -- ^ The service of interest for the health status check, either "EC2" for
      -- Amazon EC2 or "ELB" for Elastic Load Balancing.
    , asgInstances :: [Instance]
      -- ^ Provides a summary list of Amazon EC2 instances.
    , asgLaunchConfigurationName :: !Text
      -- ^ Specifies the name of the associated LaunchConfiguration.
    , asgLoadBalancerNames :: [Text]
      -- ^ A list of load balancers associated with this Auto Scaling group.
    , asgMaxSize :: !Int
      -- ^ Contains the maximum size of the Auto Scaling group.
    , asgMinSize :: !Int
      -- ^ Contains the minimum size of the Auto Scaling group.
    , asgPlacementGroup :: Maybe Text
      -- ^ The name of the cluster placement group, if applicable. For more
      -- information, go to Using Cluster Instances in the Amazon EC2 User Guide.
    , asgStatus :: Maybe Text
      -- ^ The current state of the Auto Scaling group when a DeleteAutoScalingGroup
      -- action is in progress.
    , asgSuspendedProcesses :: [SuspendedProcess]
      -- ^ Suspended processes associated with this Auto Scaling group.
    , asgTags :: [TagDescription]
      -- ^ A list of tags for the Auto Scaling group.
    , asgTerminationPolicies :: [Text]
      -- ^ A standalone termination policy or a list of termination policies for this
      -- Auto Scaling group.
    , asgvpcZoneIdentifier :: Maybe Text
      -- ^ The subnet identifier for the Amazon VPC connection, if applicable. You can
      -- specify several subnets in a comma-separated list. When you specify
      -- VPCZoneIdentifier with AvailabilityZones, ensure that the subnets'
      -- Availability Zones match the values you specify for AvailabilityZones.
    } deriving (Eq, Show, Generic)

instance ToQuery AutoScalingGroup

instance FromXML AutoScalingGroup where
    fromXMLOptions = xmlOptions

instance ToXML AutoScalingGroup where
    toXMLOptions = xmlOptions

-- | The Alarm data type.
data Alarm = Alarm
    { bAlarmARN :: Maybe ResourceName
      -- ^ The Amazon Resource Name (ARN) of the alarm.
    , bAlarmName :: Maybe Text
      -- ^ The name of the alarm.
    } deriving (Eq, Show, Generic)

instance ToQuery Alarm

instance FromXML Alarm where
    fromXMLOptions = xmlOptions

instance ToXML Alarm where
    toXMLOptions = xmlOptions

-- | Specifies whether the PutScalingPolicy ScalingAdjustment parameter is an
-- absolute number or a percentage of the current capacity.
newtype AdjustmentType = AdjustmentType
    { atAdjustmentType :: Maybe Text
      -- ^ A policy adjustment type. Valid values are ChangeInCapacity, ExactCapacity,
      -- and PercentChangeInCapacity.
    } deriving (Eq, Show, Generic)

instance ToQuery AdjustmentType

instance FromXML AdjustmentType where
    fromXMLOptions = xmlOptions

instance ToXML AdjustmentType where
    toXMLOptions = xmlOptions

-- | A scaling Activity.
data Activity = Activity
    { aActivityId :: !Text
      -- ^ Specifies the ID of the activity.
    , aAutoScalingGroupName :: !Text
      -- ^ The name of the Auto Scaling group.
    , aCause :: !Text
      -- ^ Contains the reason the activity was begun.
    , aDescription :: Maybe Text
      -- ^ Contains a friendly, more verbose description of the scaling activity.
    , aDetails :: Maybe Text
      -- ^ Contains details of the scaling activity.
    , aEndTime :: Maybe UTCTime
      -- ^ Provides the end time of this activity.
    , aProgress :: Maybe Int
      -- ^ Specifies a value between 0 and 100 that indicates the progress of the
      -- activity.
    , aStartTime :: !UTCTime
      -- ^ Provides the start time of this activity.
    , aStatusCode :: !ScalingActivityStatusCode
      -- ^ Contains the current status of the activity.
    , aStatusMessage :: Maybe Text
      -- ^ Contains a friendly, more verbose description of the activity status.
    } deriving (Eq, Show, Generic)

instance ToQuery Activity

instance FromXML Activity where
    fromXMLOptions = xmlOptions

instance ToXML Activity where
    toXMLOptions = xmlOptions

-- | Contains the current status of the activity.
data ScalingActivityStatusCode
    = ScalingActivityStatusCodeCancelled
    | ScalingActivityStatusCodeFailed
    | ScalingActivityStatusCodeInProgress
    | ScalingActivityStatusCodePreInService
    | ScalingActivityStatusCodeSuccessful
    | ScalingActivityStatusCodeWaitingForInstanceId
    | ScalingActivityStatusCodeWaitingForSpotInstanceId
    | ScalingActivityStatusCodeWaitingForSpotInstanceRequestId
      deriving (Eq, Ord, Generic)

instance Hashable ScalingActivityStatusCode

instance FromText ScalingActivityStatusCode where
    fromText "Cancelled" = Right ScalingActivityStatusCodeCancelled
    fromText "Failed" = Right ScalingActivityStatusCodeFailed
    fromText "InProgress" = Right ScalingActivityStatusCodeInProgress
    fromText "PreInService" = Right ScalingActivityStatusCodePreInService
    fromText "Successful" = Right ScalingActivityStatusCodeSuccessful
    fromText "WaitingForInstanceId" = Right ScalingActivityStatusCodeWaitingForInstanceId
    fromText "WaitingForSpotInstanceId" = Right ScalingActivityStatusCodeWaitingForSpotInstanceId
    fromText "WaitingForSpotInstanceRequestId" = Right ScalingActivityStatusCodeWaitingForSpotInstanceRequestId
    fromText e = fromTextFail $ "Unrecognised ScalingActivityStatusCode: " <> e

instance Read ScalingActivityStatusCode where
    readsPrec _ = fromTextRead

instance ToText ScalingActivityStatusCode where
    toText ScalingActivityStatusCodeCancelled = "Cancelled"
    toText ScalingActivityStatusCodeFailed = "Failed"
    toText ScalingActivityStatusCodeInProgress = "InProgress"
    toText ScalingActivityStatusCodePreInService = "PreInService"
    toText ScalingActivityStatusCodeSuccessful = "Successful"
    toText ScalingActivityStatusCodeWaitingForInstanceId = "WaitingForInstanceId"
    toText ScalingActivityStatusCodeWaitingForSpotInstanceId = "WaitingForSpotInstanceId"
    toText ScalingActivityStatusCodeWaitingForSpotInstanceRequestId = "WaitingForSpotInstanceRequestId"

instance Show ScalingActivityStatusCode where
    show = toTextShow

instance ToQuery ScalingActivityStatusCode where
    toQuery = toTextQuery

instance FromXML ScalingActivityStatusCode where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ScalingActivityStatusCode where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | Contains a description of the current lifecycle state. The Quarantined
-- lifecycle state is currently not used.
data LifecycleState
    = LifecycleStateInService
    | LifecycleStatePending
    | LifecycleStateQuarantined
    | LifecycleStateTerminated
    | LifecycleStateTerminating
      deriving (Eq, Ord, Generic)

instance Hashable LifecycleState

instance FromText LifecycleState where
    fromText "InService" = Right LifecycleStateInService
    fromText "Pending" = Right LifecycleStatePending
    fromText "Quarantined" = Right LifecycleStateQuarantined
    fromText "Terminated" = Right LifecycleStateTerminated
    fromText "Terminating" = Right LifecycleStateTerminating
    fromText e = fromTextFail $ "Unrecognised LifecycleState: " <> e

instance Read LifecycleState where
    readsPrec _ = fromTextRead

instance ToText LifecycleState where
    toText LifecycleStateInService = "InService"
    toText LifecycleStatePending = "Pending"
    toText LifecycleStateQuarantined = "Quarantined"
    toText LifecycleStateTerminated = "Terminated"
    toText LifecycleStateTerminating = "Terminating"

instance Show LifecycleState where
    show = toTextShow

instance ToQuery LifecycleState where
    toQuery = toTextQuery

instance FromXML LifecycleState where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML LifecycleState where
    toXMLOptions = xmlOptions
    toXML        = toTextXML
