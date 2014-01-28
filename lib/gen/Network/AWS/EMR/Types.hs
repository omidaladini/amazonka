{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.EMR.Types where

import Data.Aeson
import Data.ByteString      (ByteString)
import Data.Hashable
import Data.Monoid
import Data.List.NonEmpty
import Data.Text            (Text)
import Data.HashMap.Strict  (HashMap)
import Data.Time
import GHC.Generics         (Generic)
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.EMR.Service

-- | A key/value pair that contains user-defined metadata that you can associate
-- with an Amazon EMR resource. Tags make it easier to associate clusters in
-- various ways, such as grouping clusters to track your Amazon EMR resource
-- allocation costs. For more information, see Tagging Amazon EMR Resources.
data Tag = Tag
    { tKey :: Maybe Text
      -- ^ A user-defined key, which is the minimum required information for a valid
      -- tag. For more information, see Tagging Amazon EMR Resources.
    , tValue :: Maybe Text
      -- ^ A user-defined value, which is optional in a tag. For more information, see
      -- Tagging Amazon EMR Resources.
    } deriving (Eq, Show, Generic)

instance FromJSON Tag
instance ToJSON Tag

-- | The list of supported product configurations which allow user-supplied
-- arguments. EMR accepts these arguments and forwards them to the
-- corresponding installation script as bootstrap action arguments.
data SupportedProductConfig = SupportedProductConfig
    { spcArgs :: [Text]
      -- ^ The list of user-supplied arguments.
    , spcName :: Maybe Text
      -- ^ The name of the product configuration.
    } deriving (Eq, Show, Generic)

instance FromJSON SupportedProductConfig
instance ToJSON SupportedProductConfig

-- | The timeline of the cluster step status over time.
data StepTimeline = StepTimeline
    { suCreationDateTime :: Maybe UTCTime
      -- ^ The date and time when the cluster step was created.
    , suEndDateTime :: Maybe UTCTime
      -- ^ The date and time when the cluster step execution completed or failed. This
      -- can display a time that pre-dates a call to DescribeStep that indicates the
      -- step is running, due to delays in step status reporting.
    , suStartDateTime :: Maybe UTCTime
      -- ^ The date and time when the cluster step execution started. Due to delays in
      -- step status reporting, this can display a time which pre-dates a previous
      -- call to DescribeStep that indicated the step was not yet running.
    } deriving (Eq, Show, Generic)

instance FromJSON StepTimeline
instance ToJSON StepTimeline

-- | The summary of the cluster step.
data StepSummary = StepSummary
    { stId :: Maybe Text
      -- ^ The identifier of the cluster step.
    , stName :: Maybe Text
      -- ^ The name of the cluster step.
    , stStatus :: Maybe StepStatus
      -- ^ The current execution status details of the cluster step.
    } deriving (Eq, Show, Generic)

instance FromJSON StepSummary
instance ToJSON StepSummary

-- | The current execution status details of the cluster step.
data StepStatus = StepStatus
    { ssState :: Maybe StepState
      -- ^ The execution state of the cluster step.
    , ssStateChangeReason :: Maybe StepStateChangeReason
      -- ^ The reason for the step execution status change.
    , ssTimeline :: Maybe StepTimeline
      -- ^ The timeline of the cluster step status over time.
    } deriving (Eq, Show, Generic)

instance FromJSON StepStatus
instance ToJSON StepStatus

-- | The reason for the step execution status change.
data StepStateChangeReason = StepStateChangeReason
    { sscrCode :: Maybe StepStateChangeReasonCode
      -- ^ The programmable code for the state change reason.
    , sscrMessage :: Maybe Text
      -- ^ The descriptive message for the state change reason.
    } deriving (Eq, Show, Generic)

instance FromJSON StepStateChangeReason
instance ToJSON StepStateChangeReason

-- | The description of the step status.
data StepExecutionStatusDetail = StepExecutionStatusDetail
    { sesdCreationDateTime :: !UTCTime
      -- ^ The creation date and time of the step.
    , sesdEndDateTime :: Maybe UTCTime
      -- ^ The completion date and time of the step.
    , sesdLastStateChangeReason :: Maybe Text
      -- ^ A description of the step's current state.
    , sesdStartDateTime :: Maybe UTCTime
      -- ^ The start date and time of the step.
    , sesdState :: !StepExecutionState
      -- ^ The state of the job flow step.
    } deriving (Eq, Show, Generic)

instance FromJSON StepExecutionStatusDetail
instance ToJSON StepExecutionStatusDetail

-- | Combines the execution state and configuration of a step.
data StepDetail = StepDetail
    { sdExecutionStatusDetail :: StepExecutionStatusDetail
      -- ^ The description of the step status.
    , sdStepConfig :: StepConfig
      -- ^ The step configuration.
    } deriving (Eq, Show, Generic)

instance FromJSON StepDetail
instance ToJSON StepDetail

-- | Specification of a job flow step.
data StepConfig = StepConfig
    { scActionOnFailure :: Maybe ActionOnFailure
      -- ^ The action to take if the job flow step fails.
    , scHadoopJarStep :: HadoopJarStepConfig
      -- ^ The JAR file used for the job flow step.
    , scName :: !Text
      -- ^ The name of the job flow step.
    } deriving (Eq, Show, Generic)

instance FromJSON StepConfig
instance ToJSON StepConfig

-- | The step details for the requested step identifier.
data Step = Step
    { sActionOnFailure :: Maybe ActionOnFailure
      -- ^ This specifies what action to take when the cluster step fails.
      -- TERMINATE_JOB_FLOW is deprecated, use TERMINATE_CLUSTER instead.
    , sConfig :: Maybe HadoopStepConfig
      -- ^ The Hadoop job configuration of the cluster step.
    , sId :: Maybe Text
      -- ^ The identifier of the cluster step.
    , sName :: Maybe Text
      -- ^ The name of the cluster step.
    , sStatus :: Maybe StepStatus
      -- ^ The current execution status details of the cluster step.
    } deriving (Eq, Show, Generic)

instance FromJSON Step
instance ToJSON Step

-- | The script run by the bootstrap action.
data ScriptBootstrapActionConfig = ScriptBootstrapActionConfig
    { sbacArgs :: [Text]
      -- ^ A list of command line arguments to pass to the bootstrap action script.
    , sbacPath :: !Text
      -- ^ Location of the script to run during a bootstrap action. Can be either a
      -- location in Amazon S3 or on a local file system.
    } deriving (Eq, Show, Generic)

instance FromJSON ScriptBootstrapActionConfig
instance ToJSON ScriptBootstrapActionConfig

-- | The Availability Zone the job flow will run in.
newtype PlacementType = PlacementType
    { ptAvailabilityZone :: Text
      -- ^ The Amazon EC2 Availability Zone for the job flow.
    } deriving (Eq, Show, Generic)

instance FromJSON PlacementType
instance ToJSON PlacementType

-- | A key value pair.
data KeyValue = KeyValue
    { kvKey :: Maybe Text
      -- ^ The unique identifier of a key value pair.
    , kvValue :: Maybe Text
      -- ^ The value part of the identified key.
    } deriving (Eq, Show, Generic)

instance FromJSON KeyValue
instance ToJSON KeyValue

-- | Describes the Amazon EC2 instances of the job flow.
data JobFlowInstancesDetail = JobFlowInstancesDetail
    { jfidEc2KeyName :: Maybe Text
      -- ^ The name of an Amazon EC2 key pair that can be used to ssh to the master
      -- node of job flow.
    , jfidEc2SubnetId :: Maybe Text
      -- ^ For job flows launched within Amazon Virtual Private Cloud, this value
      -- specifies the identifier of the subnet where the job flow was launched.
    , jfidHadoopVersion :: Maybe Text
      -- ^ The Hadoop version for the job flow.
    , jfidInstanceCount :: !Int
      -- ^ The number of Amazon EC2 instances in the cluster. If the value is 1, the
      -- same instance serves as both the master and slave node. If the value is
      -- greater than 1, one instance is the master node and all others are slave
      -- nodes.
    , jfidInstanceGroups :: [InstanceGroupDetail]
      -- ^ Details about the job flow's instance groups.
    , jfidKeepJobFlowAliveWhenNoSteps :: Maybe Bool
      -- ^ Specifies whether the job flow should terminate after completing all steps.
    , jfidMasterInstanceId :: Maybe Text
      -- ^ The Amazon EC2 instance identifier of the master node.
    , jfidMasterInstanceType :: !Text
      -- ^ The Amazon EC2 master node instance type.
    , jfidMasterPublicDnsName :: Maybe Text
      -- ^ The DNS name of the master node.
    , jfidNormalizedInstanceHours :: Maybe Int
      -- ^ An approximation of the cost of the job flow, represented in
      -- m1.small/hours. This value is incremented once for every hour an m1.small
      -- runs. Larger instances are weighted more, so an Amazon EC2 instance that is
      -- roughly four times more expensive would result in the normalized instance
      -- hours being incremented by four. This result is only an approximation and
      -- does not reflect the actual billing rate.
    , jfidPlacement :: Maybe PlacementType
      -- ^ The Amazon EC2 Availability Zone for the job flow.
    , jfidSlaveInstanceType :: !Text
      -- ^ The Amazon EC2 slave node instance type.
    , jfidTerminationProtected :: Maybe Bool
      -- ^ Specifies whether the Amazon EC2 instances in the cluster are protected
      -- from termination by API calls, user intervention, or in the event of a job
      -- flow error.
    } deriving (Eq, Show, Generic)

instance FromJSON JobFlowInstancesDetail
instance ToJSON JobFlowInstancesDetail

-- | A specification of the number and type of Amazon EC2 instances on which to
-- run the job flow.
data JobFlowInstancesConfig = JobFlowInstancesConfig
    { jficEc2KeyName :: Maybe Text
      -- ^ The name of the Amazon EC2 key pair that can be used to ssh to the master
      -- node as the user called "hadoop.".
    , jficEc2SubnetId :: Maybe Text
      -- ^ To launch the job flow in Amazon Virtual Private Cloud (Amazon VPC), set
      -- this parameter to the identifier of the Amazon VPC subnet where you want
      -- the job flow to launch. If you do not specify this value, the job flow is
      -- launched in the normal Amazon Web Services cloud, outside of an Amazon VPC.
      -- Amazon VPC currently does not support cluster compute quadruple extra large
      -- (cc1.4xlarge) instances. Thus you cannot specify the cc1.4xlarge instance
      -- type for nodes of a job flow launched in a Amazon VPC.
    , jficHadoopVersion :: Maybe Text
      -- ^ The Hadoop version for the job flow. Valid inputs are "0.18", "0.20", or
      -- "0.20.205". If you do not set this value, the default of 0.18 is used,
      -- unless the AmiVersion parameter is set in the RunJobFlow call, in which
      -- case the default version of Hadoop for that AMI version is used.
    , jficInstanceCount :: Maybe Int
      -- ^ The number of Amazon EC2 instances used to execute the job flow.
    , jficInstanceGroups :: [InstanceGroupConfig]
      -- ^ Configuration for the job flow's instance groups.
    , jficKeepJobFlowAliveWhenNoSteps :: Maybe Bool
      -- ^ Specifies whether the job flow should terminate after completing all steps.
    , jficMasterInstanceType :: Maybe Text
      -- ^ The EC2 instance type of the master node.
    , jficPlacement :: Maybe PlacementType
      -- ^ The Availability Zone the job flow will run in.
    , jficSlaveInstanceType :: Maybe Text
      -- ^ The EC2 instance type of the slave nodes.
    , jficTerminationProtected :: Maybe Bool
      -- ^ Specifies whether to lock the job flow to prevent the Amazon EC2 instances
      -- from being terminated by API call, user intervention, or in the event of a
      -- job flow error.
    } deriving (Eq, Show, Generic)

instance FromJSON JobFlowInstancesConfig
instance ToJSON JobFlowInstancesConfig

-- | Describes the execution status of the job flow.
data JobFlowExecutionStatusDetail = JobFlowExecutionStatusDetail
    { jfesdCreationDateTime :: !UTCTime
      -- ^ The creation date and time of the job flow.
    , jfesdEndDateTime :: Maybe UTCTime
      -- ^ The completion date and time of the job flow.
    , jfesdLastStateChangeReason :: Maybe Text
      -- ^ Description of the job flow last changed state.
    , jfesdReadyDateTime :: Maybe UTCTime
      -- ^ The date and time when the job flow was ready to start running bootstrap
      -- actions.
    , jfesdStartDateTime :: Maybe UTCTime
      -- ^ The start date and time of the job flow.
    , jfesdState :: !JobFlowExecutionState
      -- ^ The state of the job flow.
    } deriving (Eq, Show, Generic)

instance FromJSON JobFlowExecutionStatusDetail
instance ToJSON JobFlowExecutionStatusDetail

-- | A description of a job flow.
data JobFlowDetail = JobFlowDetail
    { jfdAmiVersion :: Maybe Text
      -- ^ The version of the AMI used to initialize Amazon EC2 instances in the job
      -- flow. For a list of AMI versions currently supported by Amazon
      -- ElasticMapReduce, go to AMI Versions Supported in Elastic MapReduce in the
      -- Amazon Elastic MapReduce Developer's Guide.
    , jfdBootstrapActions :: [BootstrapActionDetail]
      -- ^ A list of the bootstrap actions run by the job flow.
    , jfdExecutionStatusDetail :: JobFlowExecutionStatusDetail
      -- ^ Describes the execution status of the job flow.
    , jfdInstances :: JobFlowInstancesDetail
      -- ^ Describes the Amazon EC2 instances of the job flow.
    , jfdJobFlowId :: !Text
      -- ^ The job flow identifier.
    , jfdJobFlowRole :: Maybe Text
      -- ^ The IAM role that was specified when the job flow was launched. The EC2
      -- instances of the job flow assume this role.
    , jfdLogUri :: Maybe Text
      -- ^ The location in Amazon S3 where log files for the job are stored.
    , jfdName :: !Text
      -- ^ The name of the job flow.
    , jfdSteps :: [StepDetail]
      -- ^ A list of steps run by the job flow.
    , jfdSupportedProducts :: [Text]
      -- ^ A list of strings set by third party software when the job flow is
      -- launched. If you are not using third party software to manage the job flow
      -- this value is empty.
    , jfdVisibleToAllUsers :: Maybe Bool
      -- ^ Specifies whether the job flow is visible to all IAM users of the AWS
      -- account associated with the job flow. If this value is set to true, all IAM
      -- users of that AWS account can view and (if they have the proper policy
      -- permissions set) manage the job flow. If it is set to false, only the IAM
      -- user that created the job flow can view and manage it. This value can be
      -- changed using the SetVisibleToAllUsers action.
    } deriving (Eq, Show, Generic)

instance FromJSON JobFlowDetail
instance ToJSON JobFlowDetail

-- | The timeline of the instance status over time.
data InstanceTimeline = InstanceTimeline
    { itCreationDateTime :: Maybe UTCTime
      -- ^ The creation date and time of the instance.
    , itEndDateTime :: Maybe UTCTime
      -- ^ The date and time when the instance was terminated.
    , itReadyDateTime :: Maybe UTCTime
      -- ^ The date and time when the instance was ready to perform tasks.
    } deriving (Eq, Show, Generic)

instance FromJSON InstanceTimeline
instance ToJSON InstanceTimeline

-- | The current status of the instance.
data InstanceStatus = InstanceStatus
    { isState :: Maybe InstanceState
      -- ^ The current state of the instance.
    , isStateChangeReason :: Maybe InstanceStateChangeReason
      -- ^ The details of the status change reason for the instance.
    , isTimeline :: Maybe InstanceTimeline
      -- ^ The timeline of the instance status over time.
    } deriving (Eq, Show, Generic)

instance FromJSON InstanceStatus
instance ToJSON InstanceStatus

-- | The details of the status change reason for the instance.
data InstanceStateChangeReason = InstanceStateChangeReason
    { iscrCode :: Maybe InstanceStateChangeReasonCode
      -- ^ The programmable code for the state change reason.
    , iscrMessage :: Maybe Text
      -- ^ The status change reason description.
    } deriving (Eq, Show, Generic)

instance FromJSON InstanceStateChangeReason
instance ToJSON InstanceStateChangeReason

-- | The timeline of the instance group status over time.
data InstanceGroupTimeline = InstanceGroupTimeline
    { igtCreationDateTime :: Maybe UTCTime
      -- ^ The creation date and time of the instance group.
    , igtEndDateTime :: Maybe UTCTime
      -- ^ The date and time when the instance group terminated.
    , igtReadyDateTime :: Maybe UTCTime
      -- ^ The date and time when the instance group became ready to perform tasks.
    } deriving (Eq, Show, Generic)

instance FromJSON InstanceGroupTimeline
instance ToJSON InstanceGroupTimeline

-- | The current status of the instance group.
data InstanceGroupStatus = InstanceGroupStatus
    { igsState :: Maybe InstanceGroupState
      -- ^ The current state of the instance group. The following values are
      -- deprecated: ARRESTED, SHUTTING_DOWN, and ENDED. Use SUSPENDED, TERMINATING,
      -- and TERMINATED instead, respectively.
    , igsStateChangeReason :: Maybe InstanceGroupStateChangeReason
      -- ^ The status change reason details for the instance group.
    , igsTimeline :: Maybe InstanceGroupTimeline
      -- ^ The timeline of the instance group status over time.
    } deriving (Eq, Show, Generic)

instance FromJSON InstanceGroupStatus
instance ToJSON InstanceGroupStatus

-- | The status change reason details for the instance group.
data InstanceGroupStateChangeReason = InstanceGroupStateChangeReason
    { igscrCode :: Maybe InstanceGroupStateChangeReasonCode
      -- ^ The programmable code for the state change reason.
    , igscrMessage :: Maybe Text
      -- ^ The status change reason description.
    } deriving (Eq, Show, Generic)

instance FromJSON InstanceGroupStateChangeReason
instance ToJSON InstanceGroupStateChangeReason

-- | Modify an instance group size.
data InstanceGroupModifyConfig = InstanceGroupModifyConfig
    { igmcEC2InstanceIdsToTerminate :: [Text]
      -- ^ The EC2 InstanceIds to terminate. For advanced users only. Once you
      -- terminate the instances, the instance group will not return to its original
      -- requested size.
    , igmcInstanceCount :: Maybe Int
      -- ^ Target size for the instance group.
    , igmcInstanceGroupId :: !Text
      -- ^ Unique ID of the instance group to expand or shrink.
    } deriving (Eq, Show, Generic)

instance FromJSON InstanceGroupModifyConfig
instance ToJSON InstanceGroupModifyConfig

-- | Detailed information about an instance group.
data InstanceGroupDetail = InstanceGroupDetail
    { igdBidPrice :: Maybe Text
      -- ^ Bid price for EC2 Instances when launching nodes as Spot Instances,
      -- expressed in USD.
    , igdCreationDateTime :: !UTCTime
      -- ^ The date/time the instance group was created.
    , igdEndDateTime :: Maybe UTCTime
      -- ^ The date/time the instance group was terminated.
    , igdInstanceGroupId :: Maybe Text
      -- ^ Unique identifier for the instance group.
    , igdInstanceRequestCount :: !Int
      -- ^ Target number of instances to run in the instance group.
    , igdInstanceRole :: !InstanceRoleType
      -- ^ Instance group role in the cluster.
    , igdInstanceRunningCount :: !Int
      -- ^ Actual count of running instances.
    , igdInstanceType :: !Text
      -- ^ Amazon EC2 Instance type.
    , igdLastStateChangeReason :: Maybe Text
      -- ^ Details regarding the state of the instance group.
    , igdMarket :: !MarketType
      -- ^ Market type of the Amazon EC2 instances used to create a cluster node.
    , igdName :: Maybe Text
      -- ^ Friendly name for the instance group.
    , igdReadyDateTime :: Maybe UTCTime
      -- ^ The date/time the instance group was available to the cluster.
    , igdStartDateTime :: Maybe UTCTime
      -- ^ The date/time the instance group was started.
    , igdState :: !InstanceGroupState
      -- ^ State of instance group. The following values are deprecated: STARTING,
      -- TERMINATED, and FAILED.
    } deriving (Eq, Show, Generic)

instance FromJSON InstanceGroupDetail
instance ToJSON InstanceGroupDetail

-- | Configuration defining a new instance group.
data InstanceGroupConfig = InstanceGroupConfig
    { igcBidPrice :: Maybe Text
      -- ^ Bid price for each Amazon EC2 instance in the instance group when launching
      -- nodes as Spot Instances, expressed in USD.
    , igcInstanceCount :: !Int
      -- ^ Target number of instances for the instance group.
    , igcInstanceRole :: !InstanceRoleType
      -- ^ The role of the instance group in the cluster.
    , igcInstanceType :: !Text
      -- ^ The Amazon EC2 instance type for all instances in the instance group.
    , igcMarket :: Maybe MarketType
      -- ^ Market type of the Amazon EC2 instances used to create a cluster node.
    , igcName :: Maybe Text
      -- ^ Friendly name given to the instance group.
    } deriving (Eq, Show, Generic)

instance FromJSON InstanceGroupConfig
instance ToJSON InstanceGroupConfig

-- | This entity represents an instance group, which is a group of instances
-- that have common purpose. For example, CORE instance group is used for
-- HDFS.
data InstanceGroup = InstanceGroup
    { igBidPrice :: Maybe Text
      -- ^ The bid price for each EC2 instance in the instance group when launching
      -- nodes as Spot Instances, expressed in USD.
    , igId :: Maybe Text
      -- ^ The identifier of the instance group.
    , igInstanceGroupType :: Maybe InstanceGroupType
      -- ^ The type of the instance group. Valid values are MASTER, CORE or TASK.
    , igInstanceType :: Maybe Text
      -- ^ The EC2 instance type for all instances in the instance group.
    , igMarket :: Maybe MarketType
      -- ^ The marketplace to provision instances for this group. Valid values are
      -- ON_DEMAND or SPOT.
    , igName :: Maybe Text
      -- ^ The name of the instance group.
    , igRequestedInstanceCount :: Maybe Int
      -- ^ The target number of instances for the instance group.
    , igRunningInstanceCount :: Maybe Int
      -- ^ The number of instances currently running in this instance group.
    , igStatus :: Maybe InstanceGroupStatus
      -- ^ The current status of the instance group.
    } deriving (Eq, Show, Generic)

instance FromJSON InstanceGroup
instance ToJSON InstanceGroup

-- | Represents an EC2 instance provisioned as part of cluster.
data Instance = Instance
    { iEc2InstanceId :: Maybe Text
      -- ^ The unique identifier of the instance in Amazon EC2.
    , iId :: Maybe Text
      -- ^ The unique identifier for the instance in Amazon EMR.
    , iPrivateDnsName :: Maybe Text
      -- ^ The private DNS name of the instance.
    , iPrivateIpAddress :: Maybe Text
      -- ^ The private IP address of the instance.
    , iPublicDnsName :: Maybe Text
      -- ^ The public DNS name of the instance.
    , iPublicIpAddress :: Maybe Text
      -- ^ The public IP address of the instance.
    , iStatus :: Maybe InstanceStatus
      -- ^ The current status of the instance.
    } deriving (Eq, Show, Generic)

instance FromJSON Instance
instance ToJSON Instance

-- | The Hadoop job configuration of the cluster step.
data HadoopStepConfig = HadoopStepConfig
    { hscArgs :: [Text]
      -- ^ The list of command line arguments to pass to the JAR file's main function
      -- for execution.
    , hscJar :: Maybe Text
      -- ^ The path to the JAR file that runs during the step.
    , hscMainClass :: Maybe Text
      -- ^ The name of the main class in the specified Java file. If not specified,
      -- the JAR file should specify a main class in its manifest file.
    , hscProperties :: HashMap Text Text
      -- ^ The list of Java properties that are set when the step runs. You can use
      -- these properties to pass key value pairs to your main function.
    } deriving (Eq, Show, Generic)

instance FromJSON HadoopStepConfig
instance ToJSON HadoopStepConfig

-- | The JAR file used for the job flow step.
data HadoopJarStepConfig = HadoopJarStepConfig
    { hjscArgs :: [Text]
      -- ^ A list of command line arguments passed to the JAR file's main function
      -- when executed.
    , hjscJar :: !Text
      -- ^ A path to a JAR file run during the step.
    , hjscMainClass :: Maybe Text
      -- ^ The name of the main class in the specified Java file. If not specified,
      -- the JAR file should specify a Main-Class in its manifest file.
    , hjscProperties :: [KeyValue]
      -- ^ A list of Java properties that are set when the step runs. You can use
      -- these properties to pass key value pairs to your main function.
    } deriving (Eq, Show, Generic)

instance FromJSON HadoopJarStepConfig
instance ToJSON HadoopJarStepConfig

-- | Provides information about the EC2 instances in a cluster grouped by
-- category. For example, EC2 Key Name, Subnet Id, Instance Profile, and so
-- on.
data Ec2InstanceAttributes = Ec2InstanceAttributes
    { e2iaEc2AvailabilityZone :: Maybe Text
      -- ^ The Availability Zone in which the cluster will run.
    , e2iaEc2KeyName :: Maybe Text
      -- ^ The name of the Amazon EC2 key pair to use when connecting with SSH into
      -- the master node as a user named "hadoop".
    , e2iaEc2SubnetId :: Maybe Text
      -- ^ To launch the job flow in Amazon VPC, set this parameter to the identifier
      -- of the Amazon VPC subnet where you want the job flow to launch. If you do
      -- not specify this value, the job flow is launched in the normal AWS cloud,
      -- outside of a VPC. Amazon VPC currently does not support cluster compute
      -- quadruple extra large (cc1.4xlarge) instances. Thus, you cannot specify the
      -- cc1.4xlarge instance type for nodes of a job flow launched in a VPC.
    , e2iaIamInstanceProfile :: Maybe Text
      -- ^ The IAM role that was specified when the job flow was launched. The EC2
      -- instances of the job flow assume this role.
    } deriving (Eq, Show, Generic)

instance FromJSON Ec2InstanceAttributes
instance ToJSON Ec2InstanceAttributes

-- | An entity describing an executable that runs on a cluster.
data Command = Command
    { dArgs :: [Text]
      -- ^ Arguments for Amazon EMR to pass to the command for execution.
    , dName :: Maybe Text
      -- ^ The name of the command.
    , dScriptPath :: Maybe Text
      -- ^ The Amazon S3 location of the command script.
    } deriving (Eq, Show, Generic)

instance FromJSON Command
instance ToJSON Command

-- | A timeline that represents the status of a cluster over the lifetime of the
-- cluster.
data ClusterTimeline = ClusterTimeline
    { cuCreationDateTime :: Maybe UTCTime
      -- ^ The creation date and time of the cluster.
    , cuEndDateTime :: Maybe UTCTime
      -- ^ The date and time when the cluster was terminated.
    , cuReadyDateTime :: Maybe UTCTime
      -- ^ The date and time when the cluster was ready to execute steps.
    } deriving (Eq, Show, Generic)

instance FromJSON ClusterTimeline
instance ToJSON ClusterTimeline

-- | The summary description of the cluster.
data ClusterSummary = ClusterSummary
    { ctId :: Maybe Text
      -- ^ The unique identifier for the cluster.
    , ctName :: Maybe Text
      -- ^ The name of the cluster.
    , ctStatus :: Maybe ClusterStatus
      -- ^ The details about the current status of the cluster.
    } deriving (Eq, Show, Generic)

instance FromJSON ClusterSummary
instance ToJSON ClusterSummary

-- | The current status details about the cluster.
data ClusterStatus = ClusterStatus
    { csState :: Maybe ClusterState
      -- ^ The current state of the cluster.
    , csStateChangeReason :: Maybe ClusterStateChangeReason
      -- ^ The reason for the cluster status change.
    , csTimeline :: Maybe ClusterTimeline
      -- ^ A timeline that represents the status of a cluster over the lifetime of the
      -- cluster.
    } deriving (Eq, Show, Generic)

instance FromJSON ClusterStatus
instance ToJSON ClusterStatus

-- | The reason for the cluster status change.
data ClusterStateChangeReason = ClusterStateChangeReason
    { cscrCode :: Maybe ClusterStateChangeReasonCode
      -- ^ The programmatic code for the state change reason.
    , cscrMessage :: Maybe Text
      -- ^ The descriptive message for the state change reason.
    } deriving (Eq, Show, Generic)

instance FromJSON ClusterStateChangeReason
instance ToJSON ClusterStateChangeReason

-- | This output contains the details for the requested cluster.
data Cluster = Cluster
    { cApplications :: [Application]
      -- ^ The applications installed on this cluster.
    , cAutoTerminate :: Maybe Bool
      -- ^ Specifies whether the cluster should terminate after completing all steps.
    , cEc2InstanceAttributes :: Maybe Ec2InstanceAttributes
      -- ^ Provides information about the EC2 instances in a cluster grouped by
      -- category. For example, EC2 Key Name, Subnet Id, Instance Profile, and so
      -- on.
    , cId :: Maybe Text
      -- ^ The unique identifier for the cluster.
    , cLogUri :: Maybe Text
      -- ^ The path to the Amazon S3 location where logs for this cluster are stored.
    , cName :: Maybe Text
      -- ^ The name of the cluster.
    , cRequestedAmiVersion :: Maybe Text
      -- ^ The AMI version requested for this cluster.JobFlowDetail$AmiVersion.-->.
    , cRunningAmiVersion :: Maybe Text
      -- ^ The AMI version running on this cluster. This differs from the requested
      -- version only if the requested version is a meta version, such as "latest".
      -- JobFlowDetail$AmiVersion.-->.
    , cStatus :: Maybe ClusterStatus
      -- ^ The current status details about the cluster.
    , cTags :: [Tag]
      -- ^ A list of tags associated with cluster.
    , cTerminationProtected :: Maybe Bool
      -- ^ Indicates whether Amazon EMR will lock the cluster to prevent the EC2
      -- instances from being terminated by an API call or user intervention, or in
      -- the event of a cluster error.
    , cVisibleToAllUsers :: Maybe Bool
      -- ^ Indicates whether the job flow is visible to all IAM users of the AWS
      -- account associated with the job flow. If this value is set to true, all IAM
      -- users of that AWS account can view and manage the job flow if they have the
      -- proper policy permissions set. If this value is false, only the IAM user
      -- that created the cluster can view and manage it. This value can be changed
      -- using the SetVisibleToAllUsers action.
    } deriving (Eq, Show, Generic)

instance FromJSON Cluster
instance ToJSON Cluster

-- | Reports the configuration of a bootstrap action in a job flow.
newtype BootstrapActionDetail = BootstrapActionDetail
    { badBootstrapActionConfig :: Maybe BootstrapActionConfig
      -- ^ A description of the bootstrap action.
    } deriving (Eq, Show, Generic)

instance FromJSON BootstrapActionDetail
instance ToJSON BootstrapActionDetail

-- | Configuration of a bootstrap action.
data BootstrapActionConfig = BootstrapActionConfig
    { bacName :: !Text
      -- ^ The name of the bootstrap action.
    , bacScriptBootstrapAction :: ScriptBootstrapActionConfig
      -- ^ The script run by the bootstrap action.
    } deriving (Eq, Show, Generic)

instance FromJSON BootstrapActionConfig
instance ToJSON BootstrapActionConfig

-- | An application is any Amazon or third-party software that you can add to
-- the cluster. This structure contains a list of strings that indicates the
-- software to use with the cluster and accepts a user argument list. Amazon
-- EMR accepts and forwards the argument list to the corresponding
-- installation script as bootstrap action argument. For more information, see
-- Launch a Job Flow on the MapR Distribution for Hadoop. Currently supported
-- values are: "mapr-m3" - launch the job flow using MapR M3 Edition.
-- "mapr-m5" - launch the job flow using MapR M5 Edition. "mapr" with the user
-- arguments specifying "--edition,m3" or "--edition,m5" - launch the job flow
-- using MapR M3 or M5 Edition, respectively.
data Application = Application
    { aAdditionalInfo :: HashMap Text Text
      -- ^ This option is for advanced users only. This is meta information about
      -- third-party applications that third-party vendors use for testing purposes.
    , aArgs :: [Text]
      -- ^ Arguments for Amazon EMR to pass to the application.
    , aName :: Maybe Text
      -- ^ The name of the application.
    , aVersion :: Maybe Text
      -- ^ The version of the application.
    } deriving (Eq, Show, Generic)

instance FromJSON Application
instance ToJSON Application

-- | The programmable code for the state change reason.

data StepStateChangeReasonCode
    = StepStateChangeReasonCodeNONE
      deriving (Eq, Ord, Generic)

instance Hashable StepStateChangeReasonCode

instance FromText StepStateChangeReasonCode where
    fromText "NONE" = Right StepStateChangeReasonCodeNONE
    fromText e = fromTextFail $ "Unrecognised StepStateChangeReasonCode: " <> e

instance Read StepStateChangeReasonCode where
    readsPrec _ = fromTextRead

instance ToText StepStateChangeReasonCode where
    toText StepStateChangeReasonCodeNONE = "NONE"

instance Show StepStateChangeReasonCode where
    show = toTextShow

instance FromJSON StepStateChangeReasonCode where
    parseJSON = fromTextJSON "StepStateChangeReasonCode"

instance FromJSON v => FromJSON (HashMap StepStateChangeReasonCode v) where
    parseJSON = fromTextHashJSON

instance ToJSON StepStateChangeReasonCode where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap StepStateChangeReasonCode v) where
    toJSON = toTextHashJSON

-- | The execution state of the cluster step.

data StepState
    = StepStateCANCELLED
    | StepStateCOMPLETED
    | StepStateFAILED
    | StepStateINTERRUPTED
    | StepStatePENDING
    | StepStateRUNNING
      deriving (Eq, Ord, Generic)

instance Hashable StepState

instance FromText StepState where
    fromText "CANCELLED" = Right StepStateCANCELLED
    fromText "COMPLETED" = Right StepStateCOMPLETED
    fromText "FAILED" = Right StepStateFAILED
    fromText "INTERRUPTED" = Right StepStateINTERRUPTED
    fromText "PENDING" = Right StepStatePENDING
    fromText "RUNNING" = Right StepStateRUNNING
    fromText e = fromTextFail $ "Unrecognised StepState: " <> e

instance Read StepState where
    readsPrec _ = fromTextRead

instance ToText StepState where
    toText StepStateCANCELLED = "CANCELLED"
    toText StepStateCOMPLETED = "COMPLETED"
    toText StepStateFAILED = "FAILED"
    toText StepStateINTERRUPTED = "INTERRUPTED"
    toText StepStatePENDING = "PENDING"
    toText StepStateRUNNING = "RUNNING"

instance Show StepState where
    show = toTextShow

instance FromJSON StepState where
    parseJSON = fromTextJSON "StepState"

instance FromJSON v => FromJSON (HashMap StepState v) where
    parseJSON = fromTextHashJSON

instance ToJSON StepState where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap StepState v) where
    toJSON = toTextHashJSON

-- | The state of the job flow step.

data StepExecutionState
    = StepExecutionStateCANCELLED
    | StepExecutionStateCOMPLETED
    | StepExecutionStateCONTINUE
    | StepExecutionStateFAILED
    | StepExecutionStateINTERRUPTED
    | StepExecutionStatePENDING
    | StepExecutionStateRUNNING
      deriving (Eq, Ord, Generic)

instance Hashable StepExecutionState

instance FromText StepExecutionState where
    fromText "CANCELLED" = Right StepExecutionStateCANCELLED
    fromText "COMPLETED" = Right StepExecutionStateCOMPLETED
    fromText "CONTINUE" = Right StepExecutionStateCONTINUE
    fromText "FAILED" = Right StepExecutionStateFAILED
    fromText "INTERRUPTED" = Right StepExecutionStateINTERRUPTED
    fromText "PENDING" = Right StepExecutionStatePENDING
    fromText "RUNNING" = Right StepExecutionStateRUNNING
    fromText e = fromTextFail $ "Unrecognised StepExecutionState: " <> e

instance Read StepExecutionState where
    readsPrec _ = fromTextRead

instance ToText StepExecutionState where
    toText StepExecutionStateCANCELLED = "CANCELLED"
    toText StepExecutionStateCOMPLETED = "COMPLETED"
    toText StepExecutionStateCONTINUE = "CONTINUE"
    toText StepExecutionStateFAILED = "FAILED"
    toText StepExecutionStateINTERRUPTED = "INTERRUPTED"
    toText StepExecutionStatePENDING = "PENDING"
    toText StepExecutionStateRUNNING = "RUNNING"

instance Show StepExecutionState where
    show = toTextShow

instance FromJSON StepExecutionState where
    parseJSON = fromTextJSON "StepExecutionState"

instance FromJSON v => FromJSON (HashMap StepExecutionState v) where
    parseJSON = fromTextHashJSON

instance ToJSON StepExecutionState where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap StepExecutionState v) where
    toJSON = toTextHashJSON

-- | Market type of the Amazon EC2 instances used to create a cluster node.

data MarketType
    = MarketTypeON_DEMAND
    | MarketTypeSPOT
      deriving (Eq, Ord, Generic)

instance Hashable MarketType

instance FromText MarketType where
    fromText "ON_DEMAND" = Right MarketTypeON_DEMAND
    fromText "SPOT" = Right MarketTypeSPOT
    fromText e = fromTextFail $ "Unrecognised MarketType: " <> e

instance Read MarketType where
    readsPrec _ = fromTextRead

instance ToText MarketType where
    toText MarketTypeON_DEMAND = "ON_DEMAND"
    toText MarketTypeSPOT = "SPOT"

instance Show MarketType where
    show = toTextShow

instance FromJSON MarketType where
    parseJSON = fromTextJSON "MarketType"

instance FromJSON v => FromJSON (HashMap MarketType v) where
    parseJSON = fromTextHashJSON

instance ToJSON MarketType where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap MarketType v) where
    toJSON = toTextHashJSON

-- | The type of instance. A small instance A large instance.

data JobFlowExecutionState
    = JobFlowExecutionStateBOOTSTRAPPING
    | JobFlowExecutionStateCOMPLETED
    | JobFlowExecutionStateFAILED
    | JobFlowExecutionStateRUNNING
    | JobFlowExecutionStateSHUTTING_DOWN
    | JobFlowExecutionStateSTARTING
    | JobFlowExecutionStateTERMINATED
    | JobFlowExecutionStateWAITING
      deriving (Eq, Ord, Generic)

instance Hashable JobFlowExecutionState

instance FromText JobFlowExecutionState where
    fromText "BOOTSTRAPPING" = Right JobFlowExecutionStateBOOTSTRAPPING
    fromText "COMPLETED" = Right JobFlowExecutionStateCOMPLETED
    fromText "FAILED" = Right JobFlowExecutionStateFAILED
    fromText "RUNNING" = Right JobFlowExecutionStateRUNNING
    fromText "SHUTTING_DOWN" = Right JobFlowExecutionStateSHUTTING_DOWN
    fromText "STARTING" = Right JobFlowExecutionStateSTARTING
    fromText "TERMINATED" = Right JobFlowExecutionStateTERMINATED
    fromText "WAITING" = Right JobFlowExecutionStateWAITING
    fromText e = fromTextFail $ "Unrecognised JobFlowExecutionState: " <> e

instance Read JobFlowExecutionState where
    readsPrec _ = fromTextRead

instance ToText JobFlowExecutionState where
    toText JobFlowExecutionStateBOOTSTRAPPING = "BOOTSTRAPPING"
    toText JobFlowExecutionStateCOMPLETED = "COMPLETED"
    toText JobFlowExecutionStateFAILED = "FAILED"
    toText JobFlowExecutionStateRUNNING = "RUNNING"
    toText JobFlowExecutionStateSHUTTING_DOWN = "SHUTTING_DOWN"
    toText JobFlowExecutionStateSTARTING = "STARTING"
    toText JobFlowExecutionStateTERMINATED = "TERMINATED"
    toText JobFlowExecutionStateWAITING = "WAITING"

instance Show JobFlowExecutionState where
    show = toTextShow

instance FromJSON JobFlowExecutionState where
    parseJSON = fromTextJSON "JobFlowExecutionState"

instance FromJSON v => FromJSON (HashMap JobFlowExecutionState v) where
    parseJSON = fromTextHashJSON

instance ToJSON JobFlowExecutionState where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap JobFlowExecutionState v) where
    toJSON = toTextHashJSON

-- | The programmable code for the state change reason.

data InstanceStateChangeReasonCode
    = InstanceStateChangeReasonCodeBOOTSTRAP_FAILURE
    | InstanceStateChangeReasonCodeCLUSTER_TERMINATED
    | InstanceStateChangeReasonCodeINSTANCE_FAILURE
    | InstanceStateChangeReasonCodeINTERNAL_ERROR
    | InstanceStateChangeReasonCodeVALIDATION_ERROR
      deriving (Eq, Ord, Generic)

instance Hashable InstanceStateChangeReasonCode

instance FromText InstanceStateChangeReasonCode where
    fromText "BOOTSTRAP_FAILURE" = Right InstanceStateChangeReasonCodeBOOTSTRAP_FAILURE
    fromText "CLUSTER_TERMINATED" = Right InstanceStateChangeReasonCodeCLUSTER_TERMINATED
    fromText "INSTANCE_FAILURE" = Right InstanceStateChangeReasonCodeINSTANCE_FAILURE
    fromText "INTERNAL_ERROR" = Right InstanceStateChangeReasonCodeINTERNAL_ERROR
    fromText "VALIDATION_ERROR" = Right InstanceStateChangeReasonCodeVALIDATION_ERROR
    fromText e = fromTextFail $ "Unrecognised InstanceStateChangeReasonCode: " <> e

instance Read InstanceStateChangeReasonCode where
    readsPrec _ = fromTextRead

instance ToText InstanceStateChangeReasonCode where
    toText InstanceStateChangeReasonCodeBOOTSTRAP_FAILURE = "BOOTSTRAP_FAILURE"
    toText InstanceStateChangeReasonCodeCLUSTER_TERMINATED = "CLUSTER_TERMINATED"
    toText InstanceStateChangeReasonCodeINSTANCE_FAILURE = "INSTANCE_FAILURE"
    toText InstanceStateChangeReasonCodeINTERNAL_ERROR = "INTERNAL_ERROR"
    toText InstanceStateChangeReasonCodeVALIDATION_ERROR = "VALIDATION_ERROR"

instance Show InstanceStateChangeReasonCode where
    show = toTextShow

instance FromJSON InstanceStateChangeReasonCode where
    parseJSON = fromTextJSON "InstanceStateChangeReasonCode"

instance FromJSON v => FromJSON (HashMap InstanceStateChangeReasonCode v) where
    parseJSON = fromTextHashJSON

instance ToJSON InstanceStateChangeReasonCode where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap InstanceStateChangeReasonCode v) where
    toJSON = toTextHashJSON

-- | The current state of the instance.

data InstanceState
    = InstanceStateAWAITING_FULFILLMENT
    | InstanceStateBOOTSTRAPPING
    | InstanceStatePROVISIONING
    | InstanceStateRUNNING
    | InstanceStateTERMINATED
      deriving (Eq, Ord, Generic)

instance Hashable InstanceState

instance FromText InstanceState where
    fromText "AWAITING_FULFILLMENT" = Right InstanceStateAWAITING_FULFILLMENT
    fromText "BOOTSTRAPPING" = Right InstanceStateBOOTSTRAPPING
    fromText "PROVISIONING" = Right InstanceStatePROVISIONING
    fromText "RUNNING" = Right InstanceStateRUNNING
    fromText "TERMINATED" = Right InstanceStateTERMINATED
    fromText e = fromTextFail $ "Unrecognised InstanceState: " <> e

instance Read InstanceState where
    readsPrec _ = fromTextRead

instance ToText InstanceState where
    toText InstanceStateAWAITING_FULFILLMENT = "AWAITING_FULFILLMENT"
    toText InstanceStateBOOTSTRAPPING = "BOOTSTRAPPING"
    toText InstanceStatePROVISIONING = "PROVISIONING"
    toText InstanceStateRUNNING = "RUNNING"
    toText InstanceStateTERMINATED = "TERMINATED"

instance Show InstanceState where
    show = toTextShow

instance FromJSON InstanceState where
    parseJSON = fromTextJSON "InstanceState"

instance FromJSON v => FromJSON (HashMap InstanceState v) where
    parseJSON = fromTextHashJSON

instance ToJSON InstanceState where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap InstanceState v) where
    toJSON = toTextHashJSON

-- | The role of the instance group in the cluster.

data InstanceRoleType
    = InstanceRoleTypeCORE
    | InstanceRoleTypeMASTER
    | InstanceRoleTypeTASK
      deriving (Eq, Ord, Generic)

instance Hashable InstanceRoleType

instance FromText InstanceRoleType where
    fromText "CORE" = Right InstanceRoleTypeCORE
    fromText "MASTER" = Right InstanceRoleTypeMASTER
    fromText "TASK" = Right InstanceRoleTypeTASK
    fromText e = fromTextFail $ "Unrecognised InstanceRoleType: " <> e

instance Read InstanceRoleType where
    readsPrec _ = fromTextRead

instance ToText InstanceRoleType where
    toText InstanceRoleTypeCORE = "CORE"
    toText InstanceRoleTypeMASTER = "MASTER"
    toText InstanceRoleTypeTASK = "TASK"

instance Show InstanceRoleType where
    show = toTextShow

instance FromJSON InstanceRoleType where
    parseJSON = fromTextJSON "InstanceRoleType"

instance FromJSON v => FromJSON (HashMap InstanceRoleType v) where
    parseJSON = fromTextHashJSON

instance ToJSON InstanceRoleType where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap InstanceRoleType v) where
    toJSON = toTextHashJSON

-- | The type of the instance group. Valid values are MASTER, CORE or TASK.

data InstanceGroupType
    = InstanceGroupTypeCORE
    | InstanceGroupTypeMASTER
    | InstanceGroupTypeTASK
      deriving (Eq, Ord, Generic)

instance Hashable InstanceGroupType

instance FromText InstanceGroupType where
    fromText "CORE" = Right InstanceGroupTypeCORE
    fromText "MASTER" = Right InstanceGroupTypeMASTER
    fromText "TASK" = Right InstanceGroupTypeTASK
    fromText e = fromTextFail $ "Unrecognised InstanceGroupType: " <> e

instance Read InstanceGroupType where
    readsPrec _ = fromTextRead

instance ToText InstanceGroupType where
    toText InstanceGroupTypeCORE = "CORE"
    toText InstanceGroupTypeMASTER = "MASTER"
    toText InstanceGroupTypeTASK = "TASK"

instance Show InstanceGroupType where
    show = toTextShow

instance FromJSON InstanceGroupType where
    parseJSON = fromTextJSON "InstanceGroupType"

instance FromJSON v => FromJSON (HashMap InstanceGroupType v) where
    parseJSON = fromTextHashJSON

instance ToJSON InstanceGroupType where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap InstanceGroupType v) where
    toJSON = toTextHashJSON

-- | The programmable code for the state change reason.

data InstanceGroupStateChangeReasonCode
    = InstanceGroupStateChangeReasonCodeCLUSTER_TERMINATED
    | InstanceGroupStateChangeReasonCodeINSTANCE_FAILURE
    | InstanceGroupStateChangeReasonCodeINTERNAL_ERROR
    | InstanceGroupStateChangeReasonCodeVALIDATION_ERROR
      deriving (Eq, Ord, Generic)

instance Hashable InstanceGroupStateChangeReasonCode

instance FromText InstanceGroupStateChangeReasonCode where
    fromText "CLUSTER_TERMINATED" = Right InstanceGroupStateChangeReasonCodeCLUSTER_TERMINATED
    fromText "INSTANCE_FAILURE" = Right InstanceGroupStateChangeReasonCodeINSTANCE_FAILURE
    fromText "INTERNAL_ERROR" = Right InstanceGroupStateChangeReasonCodeINTERNAL_ERROR
    fromText "VALIDATION_ERROR" = Right InstanceGroupStateChangeReasonCodeVALIDATION_ERROR
    fromText e = fromTextFail $ "Unrecognised InstanceGroupStateChangeReasonCode: " <> e

instance Read InstanceGroupStateChangeReasonCode where
    readsPrec _ = fromTextRead

instance ToText InstanceGroupStateChangeReasonCode where
    toText InstanceGroupStateChangeReasonCodeCLUSTER_TERMINATED = "CLUSTER_TERMINATED"
    toText InstanceGroupStateChangeReasonCodeINSTANCE_FAILURE = "INSTANCE_FAILURE"
    toText InstanceGroupStateChangeReasonCodeINTERNAL_ERROR = "INTERNAL_ERROR"
    toText InstanceGroupStateChangeReasonCodeVALIDATION_ERROR = "VALIDATION_ERROR"

instance Show InstanceGroupStateChangeReasonCode where
    show = toTextShow

instance FromJSON InstanceGroupStateChangeReasonCode where
    parseJSON = fromTextJSON "InstanceGroupStateChangeReasonCode"

instance FromJSON v => FromJSON (HashMap InstanceGroupStateChangeReasonCode v) where
    parseJSON = fromTextHashJSON

instance ToJSON InstanceGroupStateChangeReasonCode where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap InstanceGroupStateChangeReasonCode v) where
    toJSON = toTextHashJSON

-- | State of instance group. The following values are deprecated: STARTING,
-- TERMINATED, and FAILED.

data InstanceGroupState
    = InstanceGroupStateARRESTED
    | InstanceGroupStateBOOTSTRAPPING
    | InstanceGroupStateENDED
    | InstanceGroupStatePROVISIONING
    | InstanceGroupStateRESIZING
    | InstanceGroupStateRUNNING
    | InstanceGroupStateSHUTTING_DOWN
    | InstanceGroupStateSUSPENDED
    | InstanceGroupStateTERMINATED
    | InstanceGroupStateTERMINATING
      deriving (Eq, Ord, Generic)

instance Hashable InstanceGroupState

instance FromText InstanceGroupState where
    fromText "ARRESTED" = Right InstanceGroupStateARRESTED
    fromText "BOOTSTRAPPING" = Right InstanceGroupStateBOOTSTRAPPING
    fromText "ENDED" = Right InstanceGroupStateENDED
    fromText "PROVISIONING" = Right InstanceGroupStatePROVISIONING
    fromText "RESIZING" = Right InstanceGroupStateRESIZING
    fromText "RUNNING" = Right InstanceGroupStateRUNNING
    fromText "SHUTTING_DOWN" = Right InstanceGroupStateSHUTTING_DOWN
    fromText "SUSPENDED" = Right InstanceGroupStateSUSPENDED
    fromText "TERMINATED" = Right InstanceGroupStateTERMINATED
    fromText "TERMINATING" = Right InstanceGroupStateTERMINATING
    fromText e = fromTextFail $ "Unrecognised InstanceGroupState: " <> e

instance Read InstanceGroupState where
    readsPrec _ = fromTextRead

instance ToText InstanceGroupState where
    toText InstanceGroupStateARRESTED = "ARRESTED"
    toText InstanceGroupStateBOOTSTRAPPING = "BOOTSTRAPPING"
    toText InstanceGroupStateENDED = "ENDED"
    toText InstanceGroupStatePROVISIONING = "PROVISIONING"
    toText InstanceGroupStateRESIZING = "RESIZING"
    toText InstanceGroupStateRUNNING = "RUNNING"
    toText InstanceGroupStateSHUTTING_DOWN = "SHUTTING_DOWN"
    toText InstanceGroupStateSUSPENDED = "SUSPENDED"
    toText InstanceGroupStateTERMINATED = "TERMINATED"
    toText InstanceGroupStateTERMINATING = "TERMINATING"

instance Show InstanceGroupState where
    show = toTextShow

instance FromJSON InstanceGroupState where
    parseJSON = fromTextJSON "InstanceGroupState"

instance FromJSON v => FromJSON (HashMap InstanceGroupState v) where
    parseJSON = fromTextHashJSON

instance ToJSON InstanceGroupState where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap InstanceGroupState v) where
    toJSON = toTextHashJSON

-- | The programmatic code for the state change reason.

data ClusterStateChangeReasonCode
    = ClusterStateChangeReasonCodeALL_STEPS_COMPLETED
    | ClusterStateChangeReasonCodeBOOTSTRAP_FAILURE
    | ClusterStateChangeReasonCodeINSTANCE_FAILURE
    | ClusterStateChangeReasonCodeINTERNAL_ERROR
    | ClusterStateChangeReasonCodeSTEP_FAILURE
    | ClusterStateChangeReasonCodeUSER_REQUEST
    | ClusterStateChangeReasonCodeVALIDATION_ERROR
      deriving (Eq, Ord, Generic)

instance Hashable ClusterStateChangeReasonCode

instance FromText ClusterStateChangeReasonCode where
    fromText "ALL_STEPS_COMPLETED" = Right ClusterStateChangeReasonCodeALL_STEPS_COMPLETED
    fromText "BOOTSTRAP_FAILURE" = Right ClusterStateChangeReasonCodeBOOTSTRAP_FAILURE
    fromText "INSTANCE_FAILURE" = Right ClusterStateChangeReasonCodeINSTANCE_FAILURE
    fromText "INTERNAL_ERROR" = Right ClusterStateChangeReasonCodeINTERNAL_ERROR
    fromText "STEP_FAILURE" = Right ClusterStateChangeReasonCodeSTEP_FAILURE
    fromText "USER_REQUEST" = Right ClusterStateChangeReasonCodeUSER_REQUEST
    fromText "VALIDATION_ERROR" = Right ClusterStateChangeReasonCodeVALIDATION_ERROR
    fromText e = fromTextFail $ "Unrecognised ClusterStateChangeReasonCode: " <> e

instance Read ClusterStateChangeReasonCode where
    readsPrec _ = fromTextRead

instance ToText ClusterStateChangeReasonCode where
    toText ClusterStateChangeReasonCodeALL_STEPS_COMPLETED = "ALL_STEPS_COMPLETED"
    toText ClusterStateChangeReasonCodeBOOTSTRAP_FAILURE = "BOOTSTRAP_FAILURE"
    toText ClusterStateChangeReasonCodeINSTANCE_FAILURE = "INSTANCE_FAILURE"
    toText ClusterStateChangeReasonCodeINTERNAL_ERROR = "INTERNAL_ERROR"
    toText ClusterStateChangeReasonCodeSTEP_FAILURE = "STEP_FAILURE"
    toText ClusterStateChangeReasonCodeUSER_REQUEST = "USER_REQUEST"
    toText ClusterStateChangeReasonCodeVALIDATION_ERROR = "VALIDATION_ERROR"

instance Show ClusterStateChangeReasonCode where
    show = toTextShow

instance FromJSON ClusterStateChangeReasonCode where
    parseJSON = fromTextJSON "ClusterStateChangeReasonCode"

instance FromJSON v => FromJSON (HashMap ClusterStateChangeReasonCode v) where
    parseJSON = fromTextHashJSON

instance ToJSON ClusterStateChangeReasonCode where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap ClusterStateChangeReasonCode v) where
    toJSON = toTextHashJSON

-- | The current state of the cluster.

data ClusterState
    = ClusterStateBOOTSTRAPPING
    | ClusterStateRUNNING
    | ClusterStateSTARTING
    | ClusterStateTERMINATED
    | ClusterStateTERMINATED_WITH_ERRORS
    | ClusterStateTERMINATING
    | ClusterStateWAITING
      deriving (Eq, Ord, Generic)

instance Hashable ClusterState

instance FromText ClusterState where
    fromText "BOOTSTRAPPING" = Right ClusterStateBOOTSTRAPPING
    fromText "RUNNING" = Right ClusterStateRUNNING
    fromText "STARTING" = Right ClusterStateSTARTING
    fromText "TERMINATED" = Right ClusterStateTERMINATED
    fromText "TERMINATED_WITH_ERRORS" = Right ClusterStateTERMINATED_WITH_ERRORS
    fromText "TERMINATING" = Right ClusterStateTERMINATING
    fromText "WAITING" = Right ClusterStateWAITING
    fromText e = fromTextFail $ "Unrecognised ClusterState: " <> e

instance Read ClusterState where
    readsPrec _ = fromTextRead

instance ToText ClusterState where
    toText ClusterStateBOOTSTRAPPING = "BOOTSTRAPPING"
    toText ClusterStateRUNNING = "RUNNING"
    toText ClusterStateSTARTING = "STARTING"
    toText ClusterStateTERMINATED = "TERMINATED"
    toText ClusterStateTERMINATED_WITH_ERRORS = "TERMINATED_WITH_ERRORS"
    toText ClusterStateTERMINATING = "TERMINATING"
    toText ClusterStateWAITING = "WAITING"

instance Show ClusterState where
    show = toTextShow

instance FromJSON ClusterState where
    parseJSON = fromTextJSON "ClusterState"

instance FromJSON v => FromJSON (HashMap ClusterState v) where
    parseJSON = fromTextHashJSON

instance ToJSON ClusterState where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap ClusterState v) where
    toJSON = toTextHashJSON

-- | The action to take if the job flow step fails.

data ActionOnFailure
    = ActionOnFailureCANCEL_AND_WAIT
    | ActionOnFailureCONTINUE
    | ActionOnFailureTERMINATE_CLUSTER
    | ActionOnFailureTERMINATE_JOB_FLOW
      deriving (Eq, Ord, Generic)

instance Hashable ActionOnFailure

instance FromText ActionOnFailure where
    fromText "CANCEL_AND_WAIT" = Right ActionOnFailureCANCEL_AND_WAIT
    fromText "CONTINUE" = Right ActionOnFailureCONTINUE
    fromText "TERMINATE_CLUSTER" = Right ActionOnFailureTERMINATE_CLUSTER
    fromText "TERMINATE_JOB_FLOW" = Right ActionOnFailureTERMINATE_JOB_FLOW
    fromText e = fromTextFail $ "Unrecognised ActionOnFailure: " <> e

instance Read ActionOnFailure where
    readsPrec _ = fromTextRead

instance ToText ActionOnFailure where
    toText ActionOnFailureCANCEL_AND_WAIT = "CANCEL_AND_WAIT"
    toText ActionOnFailureCONTINUE = "CONTINUE"
    toText ActionOnFailureTERMINATE_CLUSTER = "TERMINATE_CLUSTER"
    toText ActionOnFailureTERMINATE_JOB_FLOW = "TERMINATE_JOB_FLOW"

instance Show ActionOnFailure where
    show = toTextShow

instance FromJSON ActionOnFailure where
    parseJSON = fromTextJSON "ActionOnFailure"

instance FromJSON v => FromJSON (HashMap ActionOnFailure v) where
    parseJSON = fromTextHashJSON

instance ToJSON ActionOnFailure where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap ActionOnFailure v) where
    toJSON = toTextHashJSON
