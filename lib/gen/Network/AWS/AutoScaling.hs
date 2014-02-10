-- Module      : Network.AWS.AutoScaling
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.AutoScaling
    (
    -- * Operations
    -- ** DescribeMetricCollectionTypes
      module Network.AWS.AutoScaling.DescribeMetricCollectionTypes
    -- ** PutNotificationConfiguration
    , module Network.AWS.AutoScaling.PutNotificationConfiguration
    -- ** DescribeTags
    , module Network.AWS.AutoScaling.DescribeTags
    -- ** DeleteNotificationConfiguration
    , module Network.AWS.AutoScaling.DeleteNotificationConfiguration
    -- ** PutScalingPolicy
    , module Network.AWS.AutoScaling.PutScalingPolicy
    -- ** DeleteLaunchConfiguration
    , module Network.AWS.AutoScaling.DeleteLaunchConfiguration
    -- ** SuspendProcesses
    , module Network.AWS.AutoScaling.SuspendProcesses
    -- ** SetInstanceHealth
    , module Network.AWS.AutoScaling.SetInstanceHealth
    -- ** DescribeTerminationPolicyTypes
    , module Network.AWS.AutoScaling.DescribeTerminationPolicyTypes
    -- ** DescribeAutoScalingInstances
    , module Network.AWS.AutoScaling.DescribeAutoScalingInstances
    -- ** DisableMetricsCollection
    , module Network.AWS.AutoScaling.DisableMetricsCollection
    -- ** EnableMetricsCollection
    , module Network.AWS.AutoScaling.EnableMetricsCollection
    -- ** DescribeScalingProcessTypes
    , module Network.AWS.AutoScaling.DescribeScalingProcessTypes
    -- ** DeleteTags
    , module Network.AWS.AutoScaling.DeleteTags
    -- ** DescribeAutoScalingGroups
    , module Network.AWS.AutoScaling.DescribeAutoScalingGroups
    -- ** DeleteScheduledAction
    , module Network.AWS.AutoScaling.DeleteScheduledAction
    -- ** SetDesiredCapacity
    , module Network.AWS.AutoScaling.SetDesiredCapacity
    -- ** DescribeAutoScalingNotificationTypes
    , module Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes
    -- ** DescribeScheduledActions
    , module Network.AWS.AutoScaling.DescribeScheduledActions
    -- ** CreateOrUpdateTags
    , module Network.AWS.AutoScaling.CreateOrUpdateTags
    -- ** DeletePolicy
    , module Network.AWS.AutoScaling.DeletePolicy
    -- ** AttachInstances
    , module Network.AWS.AutoScaling.AttachInstances
    -- ** UpdateAutoScalingGroup
    , module Network.AWS.AutoScaling.UpdateAutoScalingGroup
    -- ** DeleteAutoScalingGroup
    , module Network.AWS.AutoScaling.DeleteAutoScalingGroup
    -- ** ResumeProcesses
    , module Network.AWS.AutoScaling.ResumeProcesses
    -- ** ExecutePolicy
    , module Network.AWS.AutoScaling.ExecutePolicy
    -- ** DescribeAccountLimits
    , module Network.AWS.AutoScaling.DescribeAccountLimits
    -- ** TerminateInstanceInAutoScalingGroup
    , module Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup
    -- ** PutScheduledUpdateGroupAction
    , module Network.AWS.AutoScaling.PutScheduledUpdateGroupAction
    -- ** DescribePolicies
    , module Network.AWS.AutoScaling.DescribePolicies
    -- ** DescribeLaunchConfigurations
    , module Network.AWS.AutoScaling.DescribeLaunchConfigurations
    -- ** DescribeScalingActivities
    , module Network.AWS.AutoScaling.DescribeScalingActivities
    -- ** DescribeNotificationConfigurations
    , module Network.AWS.AutoScaling.DescribeNotificationConfigurations
    -- ** DescribeAdjustmentTypes
    , module Network.AWS.AutoScaling.DescribeAdjustmentTypes
    -- ** CreateAutoScalingGroup
    , module Network.AWS.AutoScaling.CreateAutoScalingGroup
    -- ** CreateLaunchConfiguration
    , module Network.AWS.AutoScaling.CreateLaunchConfiguration

    -- * Types
    -- ** TagDescription
    , TagDescription (..)
    -- ** Tag
    , Tag (..)
    -- ** SuspendedProcess
    , SuspendedProcess (..)
    -- ** ScheduledUpdateGroupAction
    , ScheduledUpdateGroupAction (..)
    -- ** ScalingPolicy
    , ScalingPolicy (..)
    -- ** ProcessType
    , ProcessType (..)
    -- ** NotificationConfiguration
    , NotificationConfiguration (..)
    -- ** MetricGranularityType
    , MetricGranularityType (..)
    -- ** MetricCollectionType
    , MetricCollectionType (..)
    -- ** LaunchConfiguration
    , LaunchConfiguration (..)
    -- ** InstanceMonitoring
    , InstanceMonitoring (..)
    -- ** Instance
    , Instance (..)
    -- ** Filter
    , Filter (..)
    -- ** EnabledMetric
    , EnabledMetric (..)
    -- ** Ebs
    , Ebs (..)
    -- ** BlockDeviceMapping
    , BlockDeviceMapping (..)
    -- ** AutoScalingInstanceDetails
    , AutoScalingInstanceDetails (..)
    -- ** AutoScalingGroup
    , AutoScalingGroup (..)
    -- ** Alarm
    , Alarm (..)
    -- ** AdjustmentType
    , AdjustmentType (..)
    -- ** Activity
    , Activity (..)
    -- ** ScalingActivityStatusCode
    , ScalingActivityStatusCode (..)
    -- ** LifecycleState
    , LifecycleState (..)

    -- * Errors
    , AutoScalingError (..)
    ) where

import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

import Network.AWS.AutoScaling.DescribeMetricCollectionTypes
import Network.AWS.AutoScaling.PutNotificationConfiguration
import Network.AWS.AutoScaling.DescribeTags
import Network.AWS.AutoScaling.DeleteNotificationConfiguration
import Network.AWS.AutoScaling.PutScalingPolicy
import Network.AWS.AutoScaling.DeleteLaunchConfiguration
import Network.AWS.AutoScaling.SuspendProcesses
import Network.AWS.AutoScaling.SetInstanceHealth
import Network.AWS.AutoScaling.DescribeTerminationPolicyTypes
import Network.AWS.AutoScaling.DescribeAutoScalingInstances
import Network.AWS.AutoScaling.DisableMetricsCollection
import Network.AWS.AutoScaling.EnableMetricsCollection
import Network.AWS.AutoScaling.DescribeScalingProcessTypes
import Network.AWS.AutoScaling.DeleteTags
import Network.AWS.AutoScaling.DescribeAutoScalingGroups
import Network.AWS.AutoScaling.DeleteScheduledAction
import Network.AWS.AutoScaling.SetDesiredCapacity
import Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes
import Network.AWS.AutoScaling.DescribeScheduledActions
import Network.AWS.AutoScaling.CreateOrUpdateTags
import Network.AWS.AutoScaling.DeletePolicy
import Network.AWS.AutoScaling.AttachInstances
import Network.AWS.AutoScaling.UpdateAutoScalingGroup
import Network.AWS.AutoScaling.DeleteAutoScalingGroup
import Network.AWS.AutoScaling.ResumeProcesses
import Network.AWS.AutoScaling.ExecutePolicy
import Network.AWS.AutoScaling.DescribeAccountLimits
import Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup
import Network.AWS.AutoScaling.PutScheduledUpdateGroupAction
import Network.AWS.AutoScaling.DescribePolicies
import Network.AWS.AutoScaling.DescribeLaunchConfigurations
import Network.AWS.AutoScaling.DescribeScalingActivities
import Network.AWS.AutoScaling.DescribeNotificationConfigurations
import Network.AWS.AutoScaling.DescribeAdjustmentTypes
import Network.AWS.AutoScaling.CreateAutoScalingGroup
import Network.AWS.AutoScaling.CreateLaunchConfiguration
