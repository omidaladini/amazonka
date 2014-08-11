{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.EMR.V2009_03_31.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Elastic MapReduce (Amazon EMR) is a web service that makes it easy
-- to process large amounts of data efficiently. Amazon EMR uses Hadoop
-- processing combined with several AWS products to do such tasks as web
-- indexing, data mining, log file analysis, machine learning, scientific
-- simulation, and data warehousing.
module Network.AWS.EMR.V2009_03_31.Types where

import Control.Lens.TH (makeIso, makeLenses)
import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2009-03-31@) of the
-- @Amazon Elastic MapReduce@ service.
data EMR deriving (Typeable)

instance AWSService EMR where
    type Sg EMR = V4
    data Er EMR
        = EMRClient HttpException
        | EMRSerializer String
        | EMRService String
        | InternalServerError
        | InternalServerException
            { _iseMessage :: Maybe Text
            }
        | InvalidRequestException
            { _ireErrorCode :: Maybe Text
            , _ireMessage :: Maybe Text
            }

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "elasticmapreduce"
        , _svcVersion  = "2009-03-31"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er EMR)
deriving instance Generic (Er EMR)

instance AWSError (Er EMR) where
    awsError = const "EMRError"

instance AWSServiceError (Er EMR) where
    serviceError    = EMRService
    clientError     = EMRClient
    serializerError = EMRSerializer

instance Exception (Er EMR)

-- | The action to take if the job flow step fails.
data ActionOnFailure
    = ActionOnFailureCancelAndWait -- ^ CANCEL_AND_WAIT
    | ActionOnFailureContinue -- ^ CONTINUE
    | ActionOnFailureTerminateCluster -- ^ TERMINATE_CLUSTER
    | ActionOnFailureTerminateJobFlow -- ^ TERMINATE_JOB_FLOW
      deriving (Eq, Show, Generic)

instance Hashable ActionOnFailure

instance FromText ActionOnFailure where
    parser = match "CANCEL_AND_WAIT" ActionOnFailureCancelAndWait
         <|> match "CONTINUE" ActionOnFailureContinue
         <|> match "TERMINATE_CLUSTER" ActionOnFailureTerminateCluster
         <|> match "TERMINATE_JOB_FLOW" ActionOnFailureTerminateJobFlow

instance ToText ActionOnFailure where
    toText ActionOnFailureCancelAndWait = "CANCEL_AND_WAIT"
    toText ActionOnFailureContinue = "CONTINUE"
    toText ActionOnFailureTerminateCluster = "TERMINATE_CLUSTER"
    toText ActionOnFailureTerminateJobFlow = "TERMINATE_JOB_FLOW"

instance ToByteString ActionOnFailure

instance FromJSON ActionOnFailure

instance ToJSON ActionOnFailure

-- | The current state of the cluster.
data ClusterState
    = ClusterStateBootstrapping -- ^ BOOTSTRAPPING
    | ClusterStateRunning -- ^ RUNNING
    | ClusterStateStarting -- ^ STARTING
    | ClusterStateTerminated -- ^ TERMINATED
    | ClusterStateTerminatedWithErrors -- ^ TERMINATED_WITH_ERRORS
    | ClusterStateTerminating -- ^ TERMINATING
    | ClusterStateWaiting -- ^ WAITING
      deriving (Eq, Show, Generic)

instance Hashable ClusterState

instance FromText ClusterState where
    parser = match "BOOTSTRAPPING" ClusterStateBootstrapping
         <|> match "RUNNING" ClusterStateRunning
         <|> match "STARTING" ClusterStateStarting
         <|> match "TERMINATED" ClusterStateTerminated
         <|> match "TERMINATED_WITH_ERRORS" ClusterStateTerminatedWithErrors
         <|> match "TERMINATING" ClusterStateTerminating
         <|> match "WAITING" ClusterStateWaiting

instance ToText ClusterState where
    toText ClusterStateBootstrapping = "BOOTSTRAPPING"
    toText ClusterStateRunning = "RUNNING"
    toText ClusterStateStarting = "STARTING"
    toText ClusterStateTerminated = "TERMINATED"
    toText ClusterStateTerminatedWithErrors = "TERMINATED_WITH_ERRORS"
    toText ClusterStateTerminating = "TERMINATING"
    toText ClusterStateWaiting = "WAITING"

instance ToByteString ClusterState

instance FromJSON ClusterState

instance ToJSON ClusterState

-- | The programmatic code for the state change reason.
data ClusterStateChangeReasonCode
    = ClusterStateChangeReasonCodeAllStepsCompleted -- ^ ALL_STEPS_COMPLETED
    | ClusterStateChangeReasonCodeBootstrapFailure -- ^ BOOTSTRAP_FAILURE
    | ClusterStateChangeReasonCodeInstanceFailure -- ^ INSTANCE_FAILURE
    | ClusterStateChangeReasonCodeInternalError -- ^ INTERNAL_ERROR
    | ClusterStateChangeReasonCodeStepFailure -- ^ STEP_FAILURE
    | ClusterStateChangeReasonCodeUserRequest -- ^ USER_REQUEST
    | ClusterStateChangeReasonCodeValidationError -- ^ VALIDATION_ERROR
      deriving (Eq, Show, Generic)

instance Hashable ClusterStateChangeReasonCode

instance FromText ClusterStateChangeReasonCode where
    parser = match "ALL_STEPS_COMPLETED" ClusterStateChangeReasonCodeAllStepsCompleted
         <|> match "BOOTSTRAP_FAILURE" ClusterStateChangeReasonCodeBootstrapFailure
         <|> match "INSTANCE_FAILURE" ClusterStateChangeReasonCodeInstanceFailure
         <|> match "INTERNAL_ERROR" ClusterStateChangeReasonCodeInternalError
         <|> match "STEP_FAILURE" ClusterStateChangeReasonCodeStepFailure
         <|> match "USER_REQUEST" ClusterStateChangeReasonCodeUserRequest
         <|> match "VALIDATION_ERROR" ClusterStateChangeReasonCodeValidationError

instance ToText ClusterStateChangeReasonCode where
    toText ClusterStateChangeReasonCodeAllStepsCompleted = "ALL_STEPS_COMPLETED"
    toText ClusterStateChangeReasonCodeBootstrapFailure = "BOOTSTRAP_FAILURE"
    toText ClusterStateChangeReasonCodeInstanceFailure = "INSTANCE_FAILURE"
    toText ClusterStateChangeReasonCodeInternalError = "INTERNAL_ERROR"
    toText ClusterStateChangeReasonCodeStepFailure = "STEP_FAILURE"
    toText ClusterStateChangeReasonCodeUserRequest = "USER_REQUEST"
    toText ClusterStateChangeReasonCodeValidationError = "VALIDATION_ERROR"

instance ToByteString ClusterStateChangeReasonCode

instance FromJSON ClusterStateChangeReasonCode

instance ToJSON ClusterStateChangeReasonCode

-- | State of instance group. The following values are deprecated: STARTING,
-- TERMINATED, and FAILED.
data InstanceGroupState
    = InstanceGroupStateArrested -- ^ ARRESTED
    | InstanceGroupStateBootstrapping -- ^ BOOTSTRAPPING
    | InstanceGroupStateEnded -- ^ ENDED
    | InstanceGroupStateProvisioning -- ^ PROVISIONING
    | InstanceGroupStateResizing -- ^ RESIZING
    | InstanceGroupStateRunning -- ^ RUNNING
    | InstanceGroupStateShuttingDown -- ^ SHUTTING_DOWN
    | InstanceGroupStateSuspended -- ^ SUSPENDED
    | InstanceGroupStateTerminated -- ^ TERMINATED
    | InstanceGroupStateTerminating -- ^ TERMINATING
      deriving (Eq, Show, Generic)

instance Hashable InstanceGroupState

instance FromText InstanceGroupState where
    parser = match "ARRESTED" InstanceGroupStateArrested
         <|> match "BOOTSTRAPPING" InstanceGroupStateBootstrapping
         <|> match "ENDED" InstanceGroupStateEnded
         <|> match "PROVISIONING" InstanceGroupStateProvisioning
         <|> match "RESIZING" InstanceGroupStateResizing
         <|> match "RUNNING" InstanceGroupStateRunning
         <|> match "SHUTTING_DOWN" InstanceGroupStateShuttingDown
         <|> match "SUSPENDED" InstanceGroupStateSuspended
         <|> match "TERMINATED" InstanceGroupStateTerminated
         <|> match "TERMINATING" InstanceGroupStateTerminating

instance ToText InstanceGroupState where
    toText InstanceGroupStateArrested = "ARRESTED"
    toText InstanceGroupStateBootstrapping = "BOOTSTRAPPING"
    toText InstanceGroupStateEnded = "ENDED"
    toText InstanceGroupStateProvisioning = "PROVISIONING"
    toText InstanceGroupStateResizing = "RESIZING"
    toText InstanceGroupStateRunning = "RUNNING"
    toText InstanceGroupStateShuttingDown = "SHUTTING_DOWN"
    toText InstanceGroupStateSuspended = "SUSPENDED"
    toText InstanceGroupStateTerminated = "TERMINATED"
    toText InstanceGroupStateTerminating = "TERMINATING"

instance ToByteString InstanceGroupState

instance FromJSON InstanceGroupState

instance ToJSON InstanceGroupState

-- | The programmable code for the state change reason.
data InstanceGroupStateChangeReasonCode
    = InstanceGroupStateChangeReasonCodeClusterTerminated -- ^ CLUSTER_TERMINATED
    | InstanceGroupStateChangeReasonCodeInstanceFailure -- ^ INSTANCE_FAILURE
    | InstanceGroupStateChangeReasonCodeInternalError -- ^ INTERNAL_ERROR
    | InstanceGroupStateChangeReasonCodeValidationError -- ^ VALIDATION_ERROR
      deriving (Eq, Show, Generic)

instance Hashable InstanceGroupStateChangeReasonCode

instance FromText InstanceGroupStateChangeReasonCode where
    parser = match "CLUSTER_TERMINATED" InstanceGroupStateChangeReasonCodeClusterTerminated
         <|> match "INSTANCE_FAILURE" InstanceGroupStateChangeReasonCodeInstanceFailure
         <|> match "INTERNAL_ERROR" InstanceGroupStateChangeReasonCodeInternalError
         <|> match "VALIDATION_ERROR" InstanceGroupStateChangeReasonCodeValidationError

instance ToText InstanceGroupStateChangeReasonCode where
    toText InstanceGroupStateChangeReasonCodeClusterTerminated = "CLUSTER_TERMINATED"
    toText InstanceGroupStateChangeReasonCodeInstanceFailure = "INSTANCE_FAILURE"
    toText InstanceGroupStateChangeReasonCodeInternalError = "INTERNAL_ERROR"
    toText InstanceGroupStateChangeReasonCodeValidationError = "VALIDATION_ERROR"

instance ToByteString InstanceGroupStateChangeReasonCode

instance FromJSON InstanceGroupStateChangeReasonCode

instance ToJSON InstanceGroupStateChangeReasonCode

-- | The type of the instance group. Valid values are MASTER, CORE or TASK.
data InstanceGroupType
    = InstanceGroupTypeCore -- ^ CORE
    | InstanceGroupTypeMaster -- ^ MASTER
    | InstanceGroupTypeTask -- ^ TASK
      deriving (Eq, Show, Generic)

instance Hashable InstanceGroupType

instance FromText InstanceGroupType where
    parser = match "CORE" InstanceGroupTypeCore
         <|> match "MASTER" InstanceGroupTypeMaster
         <|> match "TASK" InstanceGroupTypeTask

instance ToText InstanceGroupType where
    toText InstanceGroupTypeCore = "CORE"
    toText InstanceGroupTypeMaster = "MASTER"
    toText InstanceGroupTypeTask = "TASK"

instance ToByteString InstanceGroupType

instance FromJSON InstanceGroupType

instance ToJSON InstanceGroupType

-- | The role of the instance group in the cluster.
data InstanceRoleType
    = InstanceRoleTypeCore -- ^ CORE
    | InstanceRoleTypeMaster -- ^ MASTER
    | InstanceRoleTypeTask -- ^ TASK
      deriving (Eq, Show, Generic)

instance Hashable InstanceRoleType

instance FromText InstanceRoleType where
    parser = match "CORE" InstanceRoleTypeCore
         <|> match "MASTER" InstanceRoleTypeMaster
         <|> match "TASK" InstanceRoleTypeTask

instance ToText InstanceRoleType where
    toText InstanceRoleTypeCore = "CORE"
    toText InstanceRoleTypeMaster = "MASTER"
    toText InstanceRoleTypeTask = "TASK"

instance ToByteString InstanceRoleType

instance FromJSON InstanceRoleType

instance ToJSON InstanceRoleType

-- | The current state of the instance.
data InstanceState
    = InstanceStateAwaitingFulfillment -- ^ AWAITING_FULFILLMENT
    | InstanceStateBootstrapping -- ^ BOOTSTRAPPING
    | InstanceStateProvisioning -- ^ PROVISIONING
    | InstanceStateRunning -- ^ RUNNING
    | InstanceStateTerminated -- ^ TERMINATED
      deriving (Eq, Show, Generic)

instance Hashable InstanceState

instance FromText InstanceState where
    parser = match "AWAITING_FULFILLMENT" InstanceStateAwaitingFulfillment
         <|> match "BOOTSTRAPPING" InstanceStateBootstrapping
         <|> match "PROVISIONING" InstanceStateProvisioning
         <|> match "RUNNING" InstanceStateRunning
         <|> match "TERMINATED" InstanceStateTerminated

instance ToText InstanceState where
    toText InstanceStateAwaitingFulfillment = "AWAITING_FULFILLMENT"
    toText InstanceStateBootstrapping = "BOOTSTRAPPING"
    toText InstanceStateProvisioning = "PROVISIONING"
    toText InstanceStateRunning = "RUNNING"
    toText InstanceStateTerminated = "TERMINATED"

instance ToByteString InstanceState

instance FromJSON InstanceState

instance ToJSON InstanceState

-- | The programmable code for the state change reason.
data InstanceStateChangeReasonCode
    = InstanceStateChangeReasonCodeBootstrapFailure -- ^ BOOTSTRAP_FAILURE
    | InstanceStateChangeReasonCodeClusterTerminated -- ^ CLUSTER_TERMINATED
    | InstanceStateChangeReasonCodeInstanceFailure -- ^ INSTANCE_FAILURE
    | InstanceStateChangeReasonCodeInternalError -- ^ INTERNAL_ERROR
    | InstanceStateChangeReasonCodeValidationError -- ^ VALIDATION_ERROR
      deriving (Eq, Show, Generic)

instance Hashable InstanceStateChangeReasonCode

instance FromText InstanceStateChangeReasonCode where
    parser = match "BOOTSTRAP_FAILURE" InstanceStateChangeReasonCodeBootstrapFailure
         <|> match "CLUSTER_TERMINATED" InstanceStateChangeReasonCodeClusterTerminated
         <|> match "INSTANCE_FAILURE" InstanceStateChangeReasonCodeInstanceFailure
         <|> match "INTERNAL_ERROR" InstanceStateChangeReasonCodeInternalError
         <|> match "VALIDATION_ERROR" InstanceStateChangeReasonCodeValidationError

instance ToText InstanceStateChangeReasonCode where
    toText InstanceStateChangeReasonCodeBootstrapFailure = "BOOTSTRAP_FAILURE"
    toText InstanceStateChangeReasonCodeClusterTerminated = "CLUSTER_TERMINATED"
    toText InstanceStateChangeReasonCodeInstanceFailure = "INSTANCE_FAILURE"
    toText InstanceStateChangeReasonCodeInternalError = "INTERNAL_ERROR"
    toText InstanceStateChangeReasonCodeValidationError = "VALIDATION_ERROR"

instance ToByteString InstanceStateChangeReasonCode

instance FromJSON InstanceStateChangeReasonCode

instance ToJSON InstanceStateChangeReasonCode

-- | The type of instance. A small instance A large instance.
data JobFlowExecutionState
    = JobFlowExecutionStateBootstrapping -- ^ BOOTSTRAPPING
    | JobFlowExecutionStateCompleted -- ^ COMPLETED
    | JobFlowExecutionStateFailed -- ^ FAILED
    | JobFlowExecutionStateRunning -- ^ RUNNING
    | JobFlowExecutionStateShuttingDown -- ^ SHUTTING_DOWN
    | JobFlowExecutionStateStarting -- ^ STARTING
    | JobFlowExecutionStateTerminated -- ^ TERMINATED
    | JobFlowExecutionStateWaiting -- ^ WAITING
      deriving (Eq, Show, Generic)

instance Hashable JobFlowExecutionState

instance FromText JobFlowExecutionState where
    parser = match "BOOTSTRAPPING" JobFlowExecutionStateBootstrapping
         <|> match "COMPLETED" JobFlowExecutionStateCompleted
         <|> match "FAILED" JobFlowExecutionStateFailed
         <|> match "RUNNING" JobFlowExecutionStateRunning
         <|> match "SHUTTING_DOWN" JobFlowExecutionStateShuttingDown
         <|> match "STARTING" JobFlowExecutionStateStarting
         <|> match "TERMINATED" JobFlowExecutionStateTerminated
         <|> match "WAITING" JobFlowExecutionStateWaiting

instance ToText JobFlowExecutionState where
    toText JobFlowExecutionStateBootstrapping = "BOOTSTRAPPING"
    toText JobFlowExecutionStateCompleted = "COMPLETED"
    toText JobFlowExecutionStateFailed = "FAILED"
    toText JobFlowExecutionStateRunning = "RUNNING"
    toText JobFlowExecutionStateShuttingDown = "SHUTTING_DOWN"
    toText JobFlowExecutionStateStarting = "STARTING"
    toText JobFlowExecutionStateTerminated = "TERMINATED"
    toText JobFlowExecutionStateWaiting = "WAITING"

instance ToByteString JobFlowExecutionState

instance FromJSON JobFlowExecutionState

instance ToJSON JobFlowExecutionState

-- | Market type of the Amazon EC2 instances used to create a cluster node.
data MarketType
    = MarketTypeOnDemand -- ^ ON_DEMAND
    | MarketTypeSpot -- ^ SPOT
      deriving (Eq, Show, Generic)

instance Hashable MarketType

instance FromText MarketType where
    parser = match "ON_DEMAND" MarketTypeOnDemand
         <|> match "SPOT" MarketTypeSpot

instance ToText MarketType where
    toText MarketTypeOnDemand = "ON_DEMAND"
    toText MarketTypeSpot = "SPOT"

instance ToByteString MarketType

instance FromJSON MarketType

instance ToJSON MarketType

-- | The state of the job flow step.
data StepExecutionState
    = StepExecutionStateCancelled -- ^ CANCELLED
    | StepExecutionStateCompleted -- ^ COMPLETED
    | StepExecutionStateContinue -- ^ CONTINUE
    | StepExecutionStateFailed -- ^ FAILED
    | StepExecutionStateInterrupted -- ^ INTERRUPTED
    | StepExecutionStatePending -- ^ PENDING
    | StepExecutionStateRunning -- ^ RUNNING
      deriving (Eq, Show, Generic)

instance Hashable StepExecutionState

instance FromText StepExecutionState where
    parser = match "CANCELLED" StepExecutionStateCancelled
         <|> match "COMPLETED" StepExecutionStateCompleted
         <|> match "CONTINUE" StepExecutionStateContinue
         <|> match "FAILED" StepExecutionStateFailed
         <|> match "INTERRUPTED" StepExecutionStateInterrupted
         <|> match "PENDING" StepExecutionStatePending
         <|> match "RUNNING" StepExecutionStateRunning

instance ToText StepExecutionState where
    toText StepExecutionStateCancelled = "CANCELLED"
    toText StepExecutionStateCompleted = "COMPLETED"
    toText StepExecutionStateContinue = "CONTINUE"
    toText StepExecutionStateFailed = "FAILED"
    toText StepExecutionStateInterrupted = "INTERRUPTED"
    toText StepExecutionStatePending = "PENDING"
    toText StepExecutionStateRunning = "RUNNING"

instance ToByteString StepExecutionState

instance FromJSON StepExecutionState

instance ToJSON StepExecutionState

-- | The execution state of the cluster step.
data StepState
    = StepStateCancelled -- ^ CANCELLED
    | StepStateCompleted -- ^ COMPLETED
    | StepStateFailed -- ^ FAILED
    | StepStateInterrupted -- ^ INTERRUPTED
    | StepStatePending -- ^ PENDING
    | StepStateRunning -- ^ RUNNING
      deriving (Eq, Show, Generic)

instance Hashable StepState

instance FromText StepState where
    parser = match "CANCELLED" StepStateCancelled
         <|> match "COMPLETED" StepStateCompleted
         <|> match "FAILED" StepStateFailed
         <|> match "INTERRUPTED" StepStateInterrupted
         <|> match "PENDING" StepStatePending
         <|> match "RUNNING" StepStateRunning

instance ToText StepState where
    toText StepStateCancelled = "CANCELLED"
    toText StepStateCompleted = "COMPLETED"
    toText StepStateFailed = "FAILED"
    toText StepStateInterrupted = "INTERRUPTED"
    toText StepStatePending = "PENDING"
    toText StepStateRunning = "RUNNING"

instance ToByteString StepState

instance FromJSON StepState

instance ToJSON StepState

-- | The programmable code for the state change reason.
data StepStateChangeReasonCode
    = StepStateChangeReasonCodeNone -- ^ NONE
      deriving (Eq, Show, Generic)

instance Hashable StepStateChangeReasonCode

instance FromText StepStateChangeReasonCode where
    parser = match "NONE" StepStateChangeReasonCodeNone

instance ToText StepStateChangeReasonCode where
    toText StepStateChangeReasonCodeNone = "NONE"

instance ToByteString StepStateChangeReasonCode

instance FromJSON StepStateChangeReasonCode

instance ToJSON StepStateChangeReasonCode

-- | Reports the configuration of a bootstrap action in a job flow.
newtype BootstrapActionDetail = BootstrapActionDetail
    { _badBootstrapActionConfig :: Maybe BootstrapActionConfig
      -- ^ A description of the bootstrap action.
    } deriving (Show, Generic)

instance FromJSON BootstrapActionDetail

instance ToJSON BootstrapActionDetail

-- | The Availability Zone the job flow will run in.
newtype PlacementType = PlacementType
    { _ptAvailabilityZone :: Text
      -- ^ The Amazon EC2 Availability Zone for the job flow.
    } deriving (Show, Generic)

instance FromJSON PlacementType

instance ToJSON PlacementType

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
    { _rArgs :: [Text]
      -- ^ Arguments for Amazon EMR to pass to the application.
    , _rAdditionalInfo :: HashMap Text Text
      -- ^ This option is for advanced users only. This is meta information
      -- about third-party applications that third-party vendors use for
      -- testing purposes.
    , _rName :: Maybe Text
      -- ^ The name of the application.
    , _rVersion :: Maybe Text
      -- ^ The version of the application.
    } deriving (Show, Generic)

instance FromJSON Application

-- | Configuration of a bootstrap action.
data BootstrapActionConfig = BootstrapActionConfig
    { _bacName :: Text
      -- ^ The name of the bootstrap action.
    , _bacScriptBootstrapAction :: ScriptBootstrapActionConfig
      -- ^ The script run by the bootstrap action.
    } deriving (Show, Generic)

instance FromJSON BootstrapActionConfig

instance ToJSON BootstrapActionConfig

-- | This output contains the details for the requested cluster.
data Cluster = Cluster
    { _kStatus :: Maybe ClusterStatus
      -- ^ The current status details about the cluster.
    , _kRequestedAmiVersion :: Maybe Text
      -- ^ The AMI version requested for this
      -- cluster.JobFlowDetail$AmiVersion.-->.
    , _kEc2InstanceAttributes :: Maybe Ec2InstanceAttributes
      -- ^ Provides information about the EC2 instances in a cluster grouped
      -- by category. For example, key name, subnet ID, IAM instance
      -- profile, and so on.
    , _kName :: Maybe Text
      -- ^ The name of the cluster.
    , _kLogUri :: Maybe Text
      -- ^ The path to the Amazon S3 location where logs for this cluster
      -- are stored.
    , _kId :: Maybe Text
      -- ^ The unique identifier for the cluster.
    , _kRunningAmiVersion :: Maybe Text
      -- ^ The AMI version running on this cluster. This differs from the
      -- requested version only if the requested version is a meta
      -- version, such as "latest". JobFlowDetail$AmiVersion.-->.
    , _kTerminationProtected :: Maybe Bool
      -- ^ Indicates whether Amazon EMR will lock the cluster to prevent the
      -- EC2 instances from being terminated by an API call or user
      -- intervention, or in the event of a cluster error.
    , _kVisibleToAllUsers :: Maybe Bool
      -- ^ Indicates whether the job flow is visible to all IAM users of the
      -- AWS account associated with the job flow. If this value is set to
      -- true, all IAM users of that AWS account can view and manage the
      -- job flow if they have the proper policy permissions set. If this
      -- value is false, only the IAM user that created the cluster can
      -- view and manage it. This value can be changed using the
      -- SetVisibleToAllUsers action.
    , _kAutoTerminate :: Maybe Bool
      -- ^ Specifies whether the cluster should terminate after completing
      -- all steps.
    , _kApplications :: [Application]
      -- ^ The applications installed on this cluster.
    , _kTags :: [Tag]
      -- ^ A list of tags associated with a cluster.
    , _kServiceRole :: Maybe Text
      -- ^ The IAM role that was specified when the job flow was launched.
      -- Amazon ElasticMapReduce will assume this role to work with AWS
      -- resources on your behalf.
    } deriving (Show, Generic)

instance FromJSON Cluster

-- | The reason for the cluster status change.
data ClusterStateChangeReason = ClusterStateChangeReason
    { _cscrCode :: Maybe ClusterStateChangeReasonCode
      -- ^ The programmatic code for the state change reason.
    , _cscrMessage :: Maybe Text
      -- ^ The descriptive message for the state change reason.
    } deriving (Show, Generic)

instance FromJSON ClusterStateChangeReason

instance ToJSON ClusterStateChangeReason

-- | The current status details about the cluster.
data ClusterStatus = ClusterStatus
    { _csState :: Maybe ClusterState
      -- ^ The current state of the cluster.
    , _csStateChangeReason :: Maybe ClusterStateChangeReason
      -- ^ The reason for the cluster status change.
    , _csTimeline :: Maybe ClusterTimeline
      -- ^ A timeline that represents the status of a cluster over the
      -- lifetime of the cluster.
    } deriving (Show, Generic)

instance FromJSON ClusterStatus

instance ToJSON ClusterStatus

-- | The summary description of the cluster.
data ClusterSummary = ClusterSummary
    { _cwStatus :: Maybe ClusterStatus
      -- ^ The details about the current status of the cluster.
    , _cwName :: Maybe Text
      -- ^ The name of the cluster.
    , _cwId :: Maybe Text
      -- ^ The unique identifier for the cluster.
    } deriving (Show, Generic)

instance FromJSON ClusterSummary

-- | A timeline that represents the status of a cluster over the lifetime of the
-- cluster.
data ClusterTimeline = ClusterTimeline
    { _cuReadyDateTime :: Maybe POSIX
      -- ^ The date and time when the cluster was ready to execute steps.
    , _cuCreationDateTime :: Maybe POSIX
      -- ^ The creation date and time of the cluster.
    , _cuEndDateTime :: Maybe POSIX
      -- ^ The date and time when the cluster was terminated.
    } deriving (Show, Generic)

instance FromJSON ClusterTimeline

instance ToJSON ClusterTimeline

-- | An entity describing an executable that runs on a cluster.
data Command = Command
    { _cdArgs :: [Text]
      -- ^ Arguments for Amazon EMR to pass to the command for execution.
    , _cdScriptPath :: Maybe Text
      -- ^ The Amazon S3 location of the command script.
    , _cdName :: Maybe Text
      -- ^ The name of the command.
    } deriving (Show, Generic)

instance FromJSON Command

-- | Provides information about the EC2 instances in a cluster grouped by
-- category. For example, key name, subnet ID, IAM instance profile, and so
-- on.
data Ec2InstanceAttributes = Ec2InstanceAttributes
    { _eiaEc2KeyName :: Maybe Text
      -- ^ The name of the Amazon EC2 key pair to use when connecting with
      -- SSH into the master node as a user named "hadoop".
    , _eiaIamInstanceProfile :: Maybe Text
      -- ^ The IAM role that was specified when the job flow was launched.
      -- The EC2 instances of the job flow assume this role.
    , _eiaEc2SubnetId :: Maybe Text
      -- ^ To launch the job flow in Amazon VPC, set this parameter to the
      -- identifier of the Amazon VPC subnet where you want the job flow
      -- to launch. If you do not specify this value, the job flow is
      -- launched in the normal AWS cloud, outside of a VPC. Amazon VPC
      -- currently does not support cluster compute quadruple extra large
      -- (cc1.4xlarge) instances. Thus, you cannot specify the cc1.4xlarge
      -- instance type for nodes of a job flow launched in a VPC.
    , _eiaEc2AvailabilityZone :: Maybe Text
      -- ^ The Availability Zone in which the cluster will run.
    } deriving (Show, Generic)

instance FromJSON Ec2InstanceAttributes

-- | The JAR file used for the job flow step.
data HadoopJarStepConfig = HadoopJarStepConfig
    { _hjscArgs :: [Text]
      -- ^ A list of command line arguments passed to the JAR file's main
      -- function when executed.
    , _hjscJar :: Text
      -- ^ A path to a JAR file run during the step.
    , _hjscMainClass :: Maybe Text
      -- ^ The name of the main class in the specified Java file. If not
      -- specified, the JAR file should specify a Main-Class in its
      -- manifest file.
    , _hjscProperties :: [KeyValue]
      -- ^ A list of Java properties that are set when the step runs. You
      -- can use these properties to pass key value pairs to your main
      -- function.
    } deriving (Show, Generic)

instance FromJSON HadoopJarStepConfig

instance ToJSON HadoopJarStepConfig

-- | The Hadoop job configuration of the cluster step.
data HadoopStepConfig = HadoopStepConfig
    { _hscArgs :: [Text]
      -- ^ The list of command line arguments to pass to the JAR file's main
      -- function for execution.
    , _hscJar :: Maybe Text
      -- ^ The path to the JAR file that runs during the step.
    , _hscMainClass :: Maybe Text
      -- ^ The name of the main class in the specified Java file. If not
      -- specified, the JAR file should specify a main class in its
      -- manifest file.
    , _hscProperties :: HashMap Text Text
      -- ^ The list of Java properties that are set when the step runs. You
      -- can use these properties to pass key value pairs to your main
      -- function.
    } deriving (Show, Generic)

instance FromJSON HadoopStepConfig

-- | Represents an EC2 instance provisioned as part of cluster.
data Instance = Instance
    { _ieStatus :: Maybe InstanceStatus
      -- ^ The current status of the instance.
    , _iePublicDnsName :: Maybe Text
      -- ^ The public DNS name of the instance.
    , _ieEc2InstanceId :: Maybe Text
      -- ^ The unique identifier of the instance in Amazon EC2.
    , _iePrivateIpAddress :: Maybe Text
      -- ^ The private IP address of the instance.
    , _ieId :: Maybe Text
      -- ^ The unique identifier for the instance in Amazon EMR.
    , _iePrivateDnsName :: Maybe Text
      -- ^ The private DNS name of the instance.
    , _iePublicIpAddress :: Maybe Text
      -- ^ The public IP address of the instance.
    } deriving (Show, Generic)

instance FromJSON Instance

-- | This entity represents an instance group, which is a group of instances
-- that have common purpose. For example, CORE instance group is used for
-- HDFS.
data InstanceGroup = InstanceGroup
    { _igStatus :: Maybe InstanceGroupStatus
      -- ^ The current status of the instance group.
    , _igBidPrice :: Maybe Text
      -- ^ The bid price for each EC2 instance in the instance group when
      -- launching nodes as Spot Instances, expressed in USD.
    , _igRequestedInstanceCount :: Maybe Integer
      -- ^ The target number of instances for the instance group.
    , _igRunningInstanceCount :: Maybe Integer
      -- ^ The number of instances currently running in this instance group.
    , _igInstanceGroupType :: Maybe InstanceGroupType
      -- ^ The type of the instance group. Valid values are MASTER, CORE or
      -- TASK.
    , _igInstanceType :: Maybe Text
      -- ^ The EC2 instance type for all instances in the instance group.
    , _igMarket :: Maybe MarketType
      -- ^ The marketplace to provision instances for this group. Valid
      -- values are ON_DEMAND or SPOT.
    , _igName :: Maybe Text
      -- ^ The name of the instance group.
    , _igId :: Maybe Text
      -- ^ The identifier of the instance group.
    } deriving (Show, Generic)

instance FromJSON InstanceGroup

-- | Configuration defining a new instance group.
data InstanceGroupConfig = InstanceGroupConfig
    { _igcBidPrice :: Maybe Text
      -- ^ Bid price for each Amazon EC2 instance in the instance group when
      -- launching nodes as Spot Instances, expressed in USD.
    , _igcInstanceCount :: Integer
      -- ^ Target number of instances for the instance group.
    , _igcInstanceRole :: InstanceRoleType
      -- ^ The role of the instance group in the cluster.
    , _igcInstanceType :: Text
      -- ^ The Amazon EC2 instance type for all instances in the instance
      -- group.
    , _igcMarket :: Maybe MarketType
      -- ^ Market type of the Amazon EC2 instances used to create a cluster
      -- node.
    , _igcName :: Maybe Text
      -- ^ Friendly name given to the instance group.
    } deriving (Show, Generic)

instance ToJSON InstanceGroupConfig

-- | Detailed information about an instance group.
data InstanceGroupDetail = InstanceGroupDetail
    { _igdState :: InstanceGroupState
      -- ^ State of instance group. The following values are deprecated:
      -- STARTING, TERMINATED, and FAILED.
    , _igdBidPrice :: Maybe Text
      -- ^ Bid price for EC2 Instances when launching nodes as Spot
      -- Instances, expressed in USD.
    , _igdReadyDateTime :: Maybe POSIX
      -- ^ The date/time the instance group was available to the cluster.
    , _igdInstanceRole :: InstanceRoleType
      -- ^ Instance group role in the cluster.
    , _igdInstanceType :: Text
      -- ^ Amazon EC2 Instance type.
    , _igdMarket :: MarketType
      -- ^ Market type of the Amazon EC2 instances used to create a cluster
      -- node.
    , _igdLastStateChangeReason :: Maybe Text
      -- ^ Details regarding the state of the instance group.
    , _igdName :: Maybe Text
      -- ^ Friendly name for the instance group.
    , _igdInstanceRunningCount :: Integer
      -- ^ Actual count of running instances.
    , _igdCreationDateTime :: POSIX
      -- ^ The date/time the instance group was created.
    , _igdInstanceGroupId :: Maybe Text
      -- ^ Unique identifier for the instance group.
    , _igdInstanceRequestCount :: Integer
      -- ^ Target number of instances to run in the instance group.
    , _igdEndDateTime :: Maybe POSIX
      -- ^ The date/time the instance group was terminated.
    , _igdStartDateTime :: Maybe POSIX
      -- ^ The date/time the instance group was started.
    } deriving (Show, Generic)

instance FromJSON InstanceGroupDetail

instance ToJSON InstanceGroupDetail

-- | Modify an instance group size.
data InstanceGroupModifyConfig = InstanceGroupModifyConfig
    { _igmcInstanceCount :: Maybe Integer
      -- ^ Target size for the instance group.
    , _igmcEC2InstanceIdsToTerminate :: [Text]
      -- ^ The EC2 InstanceIds to terminate. For advanced users only. Once
      -- you terminate the instances, the instance group will not return
      -- to its original requested size.
    , _igmcInstanceGroupId :: Text
      -- ^ Unique ID of the instance group to expand or shrink.
    } deriving (Show, Generic)

instance ToJSON InstanceGroupModifyConfig

-- | The status change reason details for the instance group.
data InstanceGroupStateChangeReason = InstanceGroupStateChangeReason
    { _igscrCode :: Maybe InstanceGroupStateChangeReasonCode
      -- ^ The programmable code for the state change reason.
    , _igscrMessage :: Maybe Text
      -- ^ The status change reason description.
    } deriving (Show, Generic)

instance FromJSON InstanceGroupStateChangeReason

instance ToJSON InstanceGroupStateChangeReason

-- | The current status of the instance group.
data InstanceGroupStatus = InstanceGroupStatus
    { _igtState :: Maybe InstanceGroupState
      -- ^ The current state of the instance group.
    , _igtStateChangeReason :: Maybe InstanceGroupStateChangeReason
      -- ^ The status change reason details for the instance group.
    , _igtTimeline :: Maybe InstanceGroupTimeline
      -- ^ The timeline of the instance group status over time.
    } deriving (Show, Generic)

instance FromJSON InstanceGroupStatus

instance ToJSON InstanceGroupStatus

-- | The timeline of the instance group status over time.
data InstanceGroupTimeline = InstanceGroupTimeline
    { _igvReadyDateTime :: Maybe POSIX
      -- ^ The date and time when the instance group became ready to perform
      -- tasks.
    , _igvCreationDateTime :: Maybe POSIX
      -- ^ The creation date and time of the instance group.
    , _igvEndDateTime :: Maybe POSIX
      -- ^ The date and time when the instance group terminated.
    } deriving (Show, Generic)

instance FromJSON InstanceGroupTimeline

instance ToJSON InstanceGroupTimeline

-- | The details of the status change reason for the instance.
data InstanceStateChangeReason = InstanceStateChangeReason
    { _iscrCode :: Maybe InstanceStateChangeReasonCode
      -- ^ The programmable code for the state change reason.
    , _iscrMessage :: Maybe Text
      -- ^ The status change reason description.
    } deriving (Show, Generic)

instance FromJSON InstanceStateChangeReason

instance ToJSON InstanceStateChangeReason

-- | The current status of the instance.
data InstanceStatus = InstanceStatus
    { _iiwState :: Maybe InstanceState
      -- ^ The current state of the instance.
    , _iiwStateChangeReason :: Maybe InstanceStateChangeReason
      -- ^ The details of the status change reason for the instance.
    , _iiwTimeline :: Maybe InstanceTimeline
      -- ^ The timeline of the instance status over time.
    } deriving (Show, Generic)

instance FromJSON InstanceStatus

instance ToJSON InstanceStatus

-- | The timeline of the instance status over time.
data InstanceTimeline = InstanceTimeline
    { _iihReadyDateTime :: Maybe POSIX
      -- ^ The date and time when the instance was ready to perform tasks.
    , _iihCreationDateTime :: Maybe POSIX
      -- ^ The creation date and time of the instance.
    , _iihEndDateTime :: Maybe POSIX
      -- ^ The date and time when the instance was terminated.
    } deriving (Show, Generic)

instance FromJSON InstanceTimeline

instance ToJSON InstanceTimeline

-- | A description of a job flow.
data JobFlowDetail = JobFlowDetail
    { _jfdAmiVersion :: Maybe Text
      -- ^ The version of the AMI used to initialize Amazon EC2 instances in
      -- the job flow. For a list of AMI versions currently supported by
      -- Amazon ElasticMapReduce, go to AMI Versions Supported in Elastic
      -- MapReduce in the Amazon Elastic MapReduce Developer's Guide.
    , _jfdExecutionStatusDetail :: JobFlowExecutionStatusDetail
      -- ^ Describes the execution status of the job flow.
    , _jfdJobFlowId :: Text
      -- ^ The job flow identifier.
    , _jfdSteps :: [StepDetail]
      -- ^ A list of steps run by the job flow.
    , _jfdJobFlowRole :: Maybe Text
      -- ^ The IAM role that was specified when the job flow was launched.
      -- The EC2 instances of the job flow assume this role.
    , _jfdBootstrapActions :: [BootstrapActionDetail]
      -- ^ A list of the bootstrap actions run by the job flow.
    , _jfdName :: Text
      -- ^ The name of the job flow.
    , _jfdLogUri :: Maybe Text
      -- ^ The location in Amazon S3 where log files for the job are stored.
    , _jfdInstances :: JobFlowInstancesDetail
      -- ^ Describes the Amazon EC2 instances of the job flow.
    , _jfdVisibleToAllUsers :: Maybe Bool
      -- ^ Specifies whether the job flow is visible to all IAM users of the
      -- AWS account associated with the job flow. If this value is set to
      -- true, all IAM users of that AWS account can view and (if they
      -- have the proper policy permissions set) manage the job flow. If
      -- it is set to false, only the IAM user that created the job flow
      -- can view and manage it. This value can be changed using the
      -- SetVisibleToAllUsers action.
    , _jfdSupportedProducts :: [Text]
      -- ^ A list of strings set by third party software when the job flow
      -- is launched. If you are not using third party software to manage
      -- the job flow this value is empty.
    , _jfdServiceRole :: Maybe Text
      -- ^ The IAM role that was specified when the job flow was launched.
      -- Amazon ElasticMapReduce will assume this role to work with AWS
      -- resources on your behalf.
    } deriving (Show, Generic)

instance FromJSON JobFlowDetail

-- | Describes the execution status of the job flow.
data JobFlowExecutionStatusDetail = JobFlowExecutionStatusDetail
    { _jfesdState :: JobFlowExecutionState
      -- ^ The state of the job flow.
    , _jfesdReadyDateTime :: Maybe POSIX
      -- ^ The date and time when the job flow was ready to start running
      -- bootstrap actions.
    , _jfesdLastStateChangeReason :: Maybe Text
      -- ^ Description of the job flow last changed state.
    , _jfesdCreationDateTime :: POSIX
      -- ^ The creation date and time of the job flow.
    , _jfesdEndDateTime :: Maybe POSIX
      -- ^ The completion date and time of the job flow.
    , _jfesdStartDateTime :: Maybe POSIX
      -- ^ The start date and time of the job flow.
    } deriving (Show, Generic)

instance FromJSON JobFlowExecutionStatusDetail

instance ToJSON JobFlowExecutionStatusDetail

-- | A specification of the number and type of Amazon EC2 instances on which to
-- run the job flow.
data JobFlowInstancesConfig = JobFlowInstancesConfig
    { _jficEc2KeyName :: Maybe Text
      -- ^ The name of the Amazon EC2 key pair that can be used to ssh to
      -- the master node as the user called "hadoop.".
    , _jficSlaveInstanceType :: Maybe Text
      -- ^ The EC2 instance type of the slave nodes.
    , _jficInstanceCount :: Maybe Integer
      -- ^ The number of Amazon EC2 instances used to execute the job flow.
    , _jficHadoopVersion :: Maybe Text
      -- ^ The Hadoop version for the job flow. Valid inputs are "0.18",
      -- "0.20", or "0.20.205". If you do not set this value, the default
      -- of 0.18 is used, unless the AmiVersion parameter is set in the
      -- RunJobFlow call, in which case the default version of Hadoop for
      -- that AMI version is used.
    , _jficEc2SubnetId :: Maybe Text
      -- ^ To launch the job flow in Amazon Virtual Private Cloud (Amazon
      -- VPC), set this parameter to the identifier of the Amazon VPC
      -- subnet where you want the job flow to launch. If you do not
      -- specify this value, the job flow is launched in the normal Amazon
      -- Web Services cloud, outside of an Amazon VPC. Amazon VPC
      -- currently does not support cluster compute quadruple extra large
      -- (cc1.4xlarge) instances. Thus you cannot specify the cc1.4xlarge
      -- instance type for nodes of a job flow launched in a Amazon VPC.
    , _jficMasterInstanceType :: Maybe Text
      -- ^ The EC2 instance type of the master node.
    , _jficInstanceGroups :: [InstanceGroupConfig]
      -- ^ Configuration for the job flow's instance groups.
    , _jficKeepJobFlowAliveWhenNoSteps :: Maybe Bool
      -- ^ Specifies whether the job flow should terminate after completing
      -- all steps.
    , _jficTerminationProtected :: Maybe Bool
      -- ^ Specifies whether to lock the job flow to prevent the Amazon EC2
      -- instances from being terminated by API call, user intervention,
      -- or in the event of a job flow error.
    , _jficPlacement :: Maybe PlacementType
      -- ^ The Availability Zone the job flow will run in.
    } deriving (Show, Generic)

instance ToJSON JobFlowInstancesConfig

-- | Describes the Amazon EC2 instances of the job flow.
data JobFlowInstancesDetail = JobFlowInstancesDetail
    { _jfidEc2KeyName :: Maybe Text
      -- ^ The name of an Amazon EC2 key pair that can be used to ssh to the
      -- master node of job flow.
    , _jfidSlaveInstanceType :: Text
      -- ^ The Amazon EC2 slave node instance type.
    , _jfidInstanceCount :: Integer
      -- ^ The number of Amazon EC2 instances in the cluster. If the value
      -- is 1, the same instance serves as both the master and slave node.
      -- If the value is greater than 1, one instance is the master node
      -- and all others are slave nodes.
    , _jfidNormalizedInstanceHours :: Maybe Integer
      -- ^ An approximation of the cost of the job flow, represented in
      -- m1.small/hours. This value is incremented once for every hour an
      -- m1.small runs. Larger instances are weighted more, so an Amazon
      -- EC2 instance that is roughly four times more expensive would
      -- result in the normalized instance hours being incremented by
      -- four. This result is only an approximation and does not reflect
      -- the actual billing rate.
    , _jfidHadoopVersion :: Maybe Text
      -- ^ The Hadoop version for the job flow.
    , _jfidEc2SubnetId :: Maybe Text
      -- ^ For job flows launched within Amazon Virtual Private Cloud, this
      -- value specifies the identifier of the subnet where the job flow
      -- was launched.
    , _jfidMasterInstanceType :: Text
      -- ^ The Amazon EC2 master node instance type.
    , _jfidInstanceGroups :: [InstanceGroupDetail]
      -- ^ Details about the job flow's instance groups.
    , _jfidKeepJobFlowAliveWhenNoSteps :: Maybe Bool
      -- ^ Specifies whether the job flow should terminate after completing
      -- all steps.
    , _jfidMasterInstanceId :: Maybe Text
      -- ^ The Amazon EC2 instance identifier of the master node.
    , _jfidMasterPublicDnsName :: Maybe Text
      -- ^ The DNS name of the master node.
    , _jfidTerminationProtected :: Maybe Bool
      -- ^ Specifies whether the Amazon EC2 instances in the cluster are
      -- protected from termination by API calls, user intervention, or in
      -- the event of a job flow error.
    , _jfidPlacement :: Maybe PlacementType
      -- ^ The Amazon EC2 Availability Zone for the job flow.
    } deriving (Show, Generic)

instance FromJSON JobFlowInstancesDetail

instance ToJSON JobFlowInstancesDetail

-- | A key value pair.
data KeyValue = KeyValue
    { _kvValue :: Maybe Text
      -- ^ The value part of the identified key.
    , _kvKey :: Maybe Text
      -- ^ The unique identifier of a key value pair.
    } deriving (Show, Generic)

instance FromJSON KeyValue

instance ToJSON KeyValue

-- | The script run by the bootstrap action.
data ScriptBootstrapActionConfig = ScriptBootstrapActionConfig
    { _sbacArgs :: [Text]
      -- ^ A list of command line arguments to pass to the bootstrap action
      -- script.
    , _sbacPath :: Text
      -- ^ Location of the script to run during a bootstrap action. Can be
      -- either a location in Amazon S3 or on a local file system.
    } deriving (Show, Generic)

instance FromJSON ScriptBootstrapActionConfig

instance ToJSON ScriptBootstrapActionConfig

-- | The step details for the requested step identifier.
data Step = Step
    { _vStatus :: Maybe StepStatus
      -- ^ The current execution status details of the cluster step.
    , _vActionOnFailure :: Maybe ActionOnFailure
      -- ^ This specifies what action to take when the cluster step fails.
      -- Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and
      -- CONTINUE.
    , _vConfig :: Maybe HadoopStepConfig
      -- ^ The Hadoop job configuration of the cluster step.
    , _vName :: Maybe Text
      -- ^ The name of the cluster step.
    , _vId :: Maybe Text
      -- ^ The identifier of the cluster step.
    } deriving (Show, Generic)

instance FromJSON Step

-- | Specification of a job flow step.
data StepConfig = StepConfig
    { _scActionOnFailure :: Maybe ActionOnFailure
      -- ^ The action to take if the job flow step fails.
    , _scHadoopJarStep :: HadoopJarStepConfig
      -- ^ The JAR file used for the job flow step.
    , _scName :: Text
      -- ^ The name of the job flow step.
    } deriving (Show, Generic)

instance FromJSON StepConfig

instance ToJSON StepConfig

-- | Combines the execution state and configuration of a step.
data StepDetail = StepDetail
    { _seExecutionStatusDetail :: StepExecutionStatusDetail
      -- ^ The description of the step status.
    , _seStepConfig :: StepConfig
      -- ^ The step configuration.
    } deriving (Show, Generic)

instance FromJSON StepDetail

instance ToJSON StepDetail

-- | The description of the step status.
data StepExecutionStatusDetail = StepExecutionStatusDetail
    { _sesdState :: StepExecutionState
      -- ^ The state of the job flow step.
    , _sesdLastStateChangeReason :: Maybe Text
      -- ^ A description of the step's current state.
    , _sesdCreationDateTime :: POSIX
      -- ^ The creation date and time of the step.
    , _sesdEndDateTime :: Maybe POSIX
      -- ^ The completion date and time of the step.
    , _sesdStartDateTime :: Maybe POSIX
      -- ^ The start date and time of the step.
    } deriving (Show, Generic)

instance FromJSON StepExecutionStatusDetail

instance ToJSON StepExecutionStatusDetail

-- | The reason for the step execution status change.
data StepStateChangeReason = StepStateChangeReason
    { _sscrCode :: Maybe StepStateChangeReasonCode
      -- ^ The programmable code for the state change reason.
    , _sscrMessage :: Maybe Text
      -- ^ The descriptive message for the state change reason.
    } deriving (Show, Generic)

instance FromJSON StepStateChangeReason

instance ToJSON StepStateChangeReason

-- | The current execution status details of the cluster step.
data StepStatus = StepStatus
    { _ssState :: Maybe StepState
      -- ^ The execution state of the cluster step.
    , _ssStateChangeReason :: Maybe StepStateChangeReason
      -- ^ The reason for the step execution status change.
    , _ssTimeline :: Maybe StepTimeline
      -- ^ The timeline of the cluster step status over time.
    } deriving (Show, Generic)

instance FromJSON StepStatus

instance ToJSON StepStatus

-- | The summary of the cluster step.
data StepSummary = StepSummary
    { _ssyStatus :: Maybe StepStatus
      -- ^ The current execution status details of the cluster step.
    , _ssyName :: Maybe Text
      -- ^ The name of the cluster step.
    , _ssyId :: Maybe Text
      -- ^ The identifier of the cluster step.
    } deriving (Show, Generic)

instance FromJSON StepSummary

-- | The timeline of the cluster step status over time.
data StepTimeline = StepTimeline
    { _suCreationDateTime :: Maybe POSIX
      -- ^ The date and time when the cluster step was created.
    , _suEndDateTime :: Maybe POSIX
      -- ^ The date and time when the cluster step execution completed or
      -- failed.
    , _suStartDateTime :: Maybe POSIX
      -- ^ The date and time when the cluster step execution started.
    } deriving (Show, Generic)

instance FromJSON StepTimeline

instance ToJSON StepTimeline

-- | The list of supported product configurations which allow user-supplied
-- arguments. EMR accepts these arguments and forwards them to the
-- corresponding installation script as bootstrap action arguments.
data SupportedProductConfig = SupportedProductConfig
    { _spcArgs :: [Text]
      -- ^ The list of user-supplied arguments.
    , _spcName :: Maybe Text
      -- ^ The name of the product configuration.
    } deriving (Show, Generic)

instance ToJSON SupportedProductConfig

-- | A key/value pair containing user-defined metadata that you can associate
-- with an Amazon EMR resource. Tags make it easier to associate clusters in
-- various ways, such as grouping clu\ sters to track your Amazon EMR resource
-- allocation costs. For more information, see Tagging Amazon EMR Resources.
data Tag = Tag
    { _tValue :: Maybe Text
      -- ^ A user-defined value, which is optional in a tag. For more
      -- information, see Tagging Amazon EMR Resources.
    , _tKey :: Maybe Text
      -- ^ A user-defined key, which is the minimum required information for
      -- a valid tag. For more information, see Tagging Amazon EMR
      -- Resources.
    } deriving (Show, Generic)

instance FromJSON Tag

instance ToJSON Tag

-- Newtypes
makeIso ''BootstrapActionDetail
makeIso ''PlacementType

-- Products
makeLenses ''Application
makeLenses ''BootstrapActionConfig
makeLenses ''Cluster
makeLenses ''ClusterStateChangeReason
makeLenses ''ClusterStatus
makeLenses ''ClusterSummary
makeLenses ''ClusterTimeline
makeLenses ''Command
makeLenses ''Ec2InstanceAttributes
makeLenses ''HadoopJarStepConfig
makeLenses ''HadoopStepConfig
makeLenses ''Instance
makeLenses ''InstanceGroup
makeLenses ''InstanceGroupConfig
makeLenses ''InstanceGroupDetail
makeLenses ''InstanceGroupModifyConfig
makeLenses ''InstanceGroupStateChangeReason
makeLenses ''InstanceGroupStatus
makeLenses ''InstanceGroupTimeline
makeLenses ''InstanceStateChangeReason
makeLenses ''InstanceStatus
makeLenses ''InstanceTimeline
makeLenses ''JobFlowDetail
makeLenses ''JobFlowExecutionStatusDetail
makeLenses ''JobFlowInstancesConfig
makeLenses ''JobFlowInstancesDetail
makeLenses ''KeyValue
makeLenses ''ScriptBootstrapActionConfig
makeLenses ''Step
makeLenses ''StepConfig
makeLenses ''StepDetail
makeLenses ''StepExecutionStatusDetail
makeLenses ''StepStateChangeReason
makeLenses ''StepStatus
makeLenses ''StepSummary
makeLenses ''StepTimeline
makeLenses ''SupportedProductConfig
makeLenses ''Tag