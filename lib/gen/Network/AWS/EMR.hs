-- Module      : Network.AWS.EMR
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.EMR
    (
    -- * Operations
    -- ** RunJobFlow
      module Network.AWS.EMR.RunJobFlow
    -- ** SetVisibleToAllUsers
    , module Network.AWS.EMR.SetVisibleToAllUsers
    -- ** TerminateJobFlows
    , module Network.AWS.EMR.TerminateJobFlows
    -- ** DescribeStep
    , module Network.AWS.EMR.DescribeStep
    -- ** RemoveTags
    , module Network.AWS.EMR.RemoveTags
    -- ** DescribeCluster
    , module Network.AWS.EMR.DescribeCluster
    -- ** SetTerminationProtection
    , module Network.AWS.EMR.SetTerminationProtection
    -- ** AddJobFlowSteps
    , module Network.AWS.EMR.AddJobFlowSteps
    -- ** ModifyInstanceGroups
    , module Network.AWS.EMR.ModifyInstanceGroups
    -- ** ListSteps
    , module Network.AWS.EMR.ListSteps
    -- ** DescribeJobFlows
    , module Network.AWS.EMR.DescribeJobFlows
    -- ** AddInstanceGroups
    , module Network.AWS.EMR.AddInstanceGroups
    -- ** ListInstanceGroups
    , module Network.AWS.EMR.ListInstanceGroups
    -- ** ListBootstrapActions
    , module Network.AWS.EMR.ListBootstrapActions
    -- ** AddTags
    , module Network.AWS.EMR.AddTags
    -- ** ListInstances
    , module Network.AWS.EMR.ListInstances
    -- ** ListClusters
    , module Network.AWS.EMR.ListClusters

    -- * Types
    -- ** Tag
    , Tag (..)
    -- ** SupportedProductConfig
    , SupportedProductConfig (..)
    -- ** StepTimeline
    , StepTimeline (..)
    -- ** StepSummary
    , StepSummary (..)
    -- ** StepStatus
    , StepStatus (..)
    -- ** StepStateChangeReason
    , StepStateChangeReason (..)
    -- ** StepExecutionStatusDetail
    , StepExecutionStatusDetail (..)
    -- ** StepDetail
    , StepDetail (..)
    -- ** StepConfig
    , StepConfig (..)
    -- ** Step
    , Step (..)
    -- ** ScriptBootstrapActionConfig
    , ScriptBootstrapActionConfig (..)
    -- ** PlacementType
    , PlacementType (..)
    -- ** KeyValue
    , KeyValue (..)
    -- ** JobFlowInstancesDetail
    , JobFlowInstancesDetail (..)
    -- ** JobFlowInstancesConfig
    , JobFlowInstancesConfig (..)
    -- ** JobFlowExecutionStatusDetail
    , JobFlowExecutionStatusDetail (..)
    -- ** JobFlowDetail
    , JobFlowDetail (..)
    -- ** InstanceTimeline
    , InstanceTimeline (..)
    -- ** InstanceStatus
    , InstanceStatus (..)
    -- ** InstanceStateChangeReason
    , InstanceStateChangeReason (..)
    -- ** InstanceGroupTimeline
    , InstanceGroupTimeline (..)
    -- ** InstanceGroupStatus
    , InstanceGroupStatus (..)
    -- ** InstanceGroupStateChangeReason
    , InstanceGroupStateChangeReason (..)
    -- ** InstanceGroupModifyConfig
    , InstanceGroupModifyConfig (..)
    -- ** InstanceGroupDetail
    , InstanceGroupDetail (..)
    -- ** InstanceGroupConfig
    , InstanceGroupConfig (..)
    -- ** InstanceGroup
    , InstanceGroup (..)
    -- ** Instance
    , Instance (..)
    -- ** HadoopStepConfig
    , HadoopStepConfig (..)
    -- ** HadoopJarStepConfig
    , HadoopJarStepConfig (..)
    -- ** Ec2InstanceAttributes
    , Ec2InstanceAttributes (..)
    -- ** Command
    , Command (..)
    -- ** ClusterTimeline
    , ClusterTimeline (..)
    -- ** ClusterSummary
    , ClusterSummary (..)
    -- ** ClusterStatus
    , ClusterStatus (..)
    -- ** ClusterStateChangeReason
    , ClusterStateChangeReason (..)
    -- ** Cluster
    , Cluster (..)
    -- ** BootstrapActionDetail
    , BootstrapActionDetail (..)
    -- ** BootstrapActionConfig
    , BootstrapActionConfig (..)
    -- ** Application
    , Application (..)
    -- ** StepStateChangeReasonCode
    , StepStateChangeReasonCode (..)
    -- ** StepState
    , StepState (..)
    -- ** StepExecutionState
    , StepExecutionState (..)
    -- ** MarketType
    , MarketType (..)
    -- ** JobFlowExecutionState
    , JobFlowExecutionState (..)
    -- ** InstanceStateChangeReasonCode
    , InstanceStateChangeReasonCode (..)
    -- ** InstanceState
    , InstanceState (..)
    -- ** InstanceRoleType
    , InstanceRoleType (..)
    -- ** InstanceGroupType
    , InstanceGroupType (..)
    -- ** InstanceGroupStateChangeReasonCode
    , InstanceGroupStateChangeReasonCode (..)
    -- ** InstanceGroupState
    , InstanceGroupState (..)
    -- ** ClusterStateChangeReasonCode
    , ClusterStateChangeReasonCode (..)
    -- ** ClusterState
    , ClusterState (..)
    -- ** ActionOnFailure
    , ActionOnFailure (..)

    -- * Errors
    , EMRError (..)
    ) where

import Network.AWS.EMR.Service
import Network.AWS.EMR.Types

import Network.AWS.EMR.RunJobFlow
import Network.AWS.EMR.SetVisibleToAllUsers
import Network.AWS.EMR.TerminateJobFlows
import Network.AWS.EMR.DescribeStep
import Network.AWS.EMR.RemoveTags
import Network.AWS.EMR.DescribeCluster
import Network.AWS.EMR.SetTerminationProtection
import Network.AWS.EMR.AddJobFlowSteps
import Network.AWS.EMR.ModifyInstanceGroups
import Network.AWS.EMR.ListSteps
import Network.AWS.EMR.DescribeJobFlows
import Network.AWS.EMR.AddInstanceGroups
import Network.AWS.EMR.ListInstanceGroups
import Network.AWS.EMR.ListBootstrapActions
import Network.AWS.EMR.AddTags
import Network.AWS.EMR.ListInstances
import Network.AWS.EMR.ListClusters
