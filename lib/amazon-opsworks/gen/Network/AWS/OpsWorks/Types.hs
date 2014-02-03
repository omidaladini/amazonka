{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.OpsWorks.Types where

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

import Network.AWS.OpsWorks.Service

-- | A WeeklyAutoScalingSchedule object with the instance schedule.
data WeeklyAutoScalingSchedule = WeeklyAutoScalingSchedule
    { wassFriday :: HashMap Text Text
      -- ^ The schedule for Friday.
    , wassMonday :: HashMap Text Text
      -- ^ The schedule for Monday.
    , wassSaturday :: HashMap Text Text
      -- ^ The schedule for Saturday.
    , wassSunday :: HashMap Text Text
      -- ^ The schedule for Sunday.
    , wassThursday :: HashMap Text Text
      -- ^ The schedule for Thursday.
    , wassTuesday :: HashMap Text Text
      -- ^ The schedule for Tuesday.
    , wassWednesday :: HashMap Text Text
      -- ^ The schedule for Wednesday.
    } deriving (Eq, Show, Generic)

instance FromJSON WeeklyAutoScalingSchedule
instance ToJSON WeeklyAutoScalingSchedule

-- | Describes an Amazon EBS volume configuration.
data VolumeConfiguration = VolumeConfiguration
    { vcMountPoint :: !Text
      -- ^ The volume mount point. For example "/dev/sdh".
    , vcNumberOfDisks :: !Int
      -- ^ The number of disks in the volume.
    , vcRaidLevel :: Maybe Int
      -- ^ The volume RAID level.
    , vcSize :: !Int
      -- ^ The volume size.
    } deriving (Eq, Show, Generic)

instance FromJSON VolumeConfiguration
instance ToJSON VolumeConfiguration

-- | Describes an instance's Amazon EBS volume.
data Volume = Volume
    { vAvailabilityZone :: Maybe Text
      -- ^ The volume Availability Zone. For more information, see Regions and
      -- Endpoints.
    , vDevice :: Maybe Text
      -- ^ The device name.
    , vEc2VolumeId :: Maybe Text
      -- ^ The Amazon EC2 volume ID.
    , vInstanceId :: Maybe Text
      -- ^ The instance ID.
    , vMountPoint :: Maybe Text
      -- ^ The volume mount point. For example "/dev/sdh".
    , vName :: Maybe Text
      -- ^ The volume name.
    , vRaidArrayId :: Maybe Text
      -- ^ The RAID array ID.
    , vRegion :: Maybe Text
      -- ^ The AWS region. For more information about AWS regions, see Regions and
      -- Endpoints.
    , vSize :: Maybe Int
      -- ^ The volume size.
    , vStatus :: Maybe Text
      -- ^ The value returned by DescribeVolumes.
    , vVolumeId :: Maybe Text
      -- ^ The volume ID.
    } deriving (Eq, Show, Generic)

instance FromJSON Volume
instance ToJSON Volume

-- | Describes a user's SSH information.
data UserProfile = UserProfile
    { upAllowSelfManagement :: Maybe Bool
      -- ^ Whether users can specify their own SSH public key through the My Settings
      -- page. For more information, see Managing User Permissions.
    , upIamUserArn :: Maybe Text
      -- ^ The user's IAM ARN.
    , upName :: Maybe Text
      -- ^ The user's name.
    , upSshPublicKey :: Maybe Text
      -- ^ The user's SSH public key.
    , upSshUsername :: Maybe Text
      -- ^ The user's SSH user name.
    } deriving (Eq, Show, Generic)

instance FromJSON UserProfile
instance ToJSON UserProfile

-- | Describes an instance's time-based auto scaling configuration.
data TimeBasedAutoScalingConfiguration = TimeBasedAutoScalingConfiguration
    { tbascAutoScalingSchedule :: Maybe WeeklyAutoScalingSchedule
      -- ^ A WeeklyAutoScalingSchedule object with the instance schedule.
    , tbascInstanceId :: Maybe Text
      -- ^ The instance ID.
    } deriving (Eq, Show, Generic)

instance FromJSON TimeBasedAutoScalingConfiguration
instance ToJSON TimeBasedAutoScalingConfiguration

-- | A StackSummary object that contains the results.
data StackSummary = StackSummary
    { ssAppsCount :: Maybe Int
      -- ^ The number of apps.
    , ssInstancesCount :: Maybe InstancesCount
      -- ^ An InstancesCount object with the number of instances in each status.
    , ssLayersCount :: Maybe Int
      -- ^ The number of layers.
    , ssName :: Maybe Text
      -- ^ The stack name.
    , ssStackId :: Maybe Text
      -- ^ The stack ID.
    } deriving (Eq, Show, Generic)

instance FromJSON StackSummary
instance ToJSON StackSummary

-- | The configuration manager. When you update a stack you can optionally use
-- the configuration manager to specify the Chef version, 0.9 or 11.4. If you
-- omit this parameter, AWS OpsWorks does not change the Chef version.
data StackConfigurationManager = StackConfigurationManager
    { scmName :: Maybe Text
      -- ^ The name. This parameter must be set to "Chef".
    , scmVersion :: Maybe Text
      -- ^ The Chef version. This parameter must be set to "0.9" or "11.4". The
      -- default value is "0.9". However, we expect to change the default value to
      -- "11.4" in September 2013.
    } deriving (Eq, Show, Generic)

instance FromJSON StackConfigurationManager
instance ToJSON StackConfigurationManager

-- | Describes a stack.
data Stack = Stack
    { tAttributes :: HashMap StackAttributesKeys Text
      -- ^ The contents of the stack's attributes bag.
    , tConfigurationManager :: Maybe StackConfigurationManager
      -- ^ The configuration manager.
    , tCreatedAt :: Maybe Text
      -- ^ Date when the stack was created.
    , tCustomCookbooksSource :: Maybe Source
      -- ^ Contains the information required to retrieve an app or cookbook from a
      -- repository. For more information, see Creating Apps or Custom Recipes and
      -- Cookbooks.
    , tCustomJson :: Maybe Text
      -- ^ A string that contains user-defined, custom JSON. It is used to override
      -- the corresponding default stack configuration JSON values. The string
      -- should be in the following format and must escape characters such as '"'.:
      -- "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more information on
      -- custom JSON, see Use Custom JSON to Modify the Stack Configuration JSON.
    , tDefaultAvailabilityZone :: Maybe Text
      -- ^ The stack's default Availability Zone. For more information, see Regions
      -- and Endpoints.
    , tDefaultInstanceProfileArn :: Maybe Text
      -- ^ The ARN of an IAM profile that is the default profile for all of the
      -- stack's EC2 instances. For more information about IAM ARNs, see Using
      -- Identifiers.
    , tDefaultOs :: Maybe Text
      -- ^ The stack's default operating system, which must be set to Amazon Linux or
      -- Ubuntu 12.04 LTS. The default option is Amazon Linux.
    , tDefaultRootDeviceType :: Maybe RootDeviceType
      -- ^ The default root device type. This value is used by default for all
      -- instances in the cloned stack, but you can override it when you create an
      -- instance. For more information, see Storage for the Root Device.
    , tDefaultSshKeyName :: Maybe Text
      -- ^ A default SSH key for the stack's instances. You can override this value
      -- when you create or update an instance.
    , tDefaultSubnetId :: Maybe Text
      -- ^ The default subnet ID, if the stack is running in a VPC.
    , tHostnameTheme :: Maybe Text
      -- ^ The stack host name theme, with spaces replaced by underscores.
    , tName :: Maybe Text
      -- ^ The stack name.
    , tRegion :: Maybe Text
      -- ^ The stack AWS region, such as "us-east-1". For more information about AWS
      -- regions, see Regions and Endpoints.
    , tServiceRoleArn :: Maybe Text
      -- ^ The stack AWS Identity and Access Management (IAM) role.
    , tStackId :: Maybe Text
      -- ^ The stack ID.
    , tUseCustomCookbooks :: Maybe Bool
      -- ^ Whether the stack uses custom cookbooks.
    , tVpcId :: Maybe Text
      -- ^ The VPC ID, if the stack is running in a VPC.
    } deriving (Eq, Show, Generic)

instance FromJSON Stack
instance ToJSON Stack

-- | An SslConfiguration object with the SSL configuration.
data SslConfiguration = SslConfiguration
    { scCertificate :: !Text
      -- ^ The contents of the certificate's domain.crt file.
    , scChain :: Maybe Text
      -- ^ Optional. Can be used to specify an intermediate certificate authority key
      -- or client authentication.
    , scPrivateKey :: !Text
      -- ^ The private key; the contents of the certificate's domain.kex file.
    } deriving (Eq, Show, Generic)

instance FromJSON SslConfiguration
instance ToJSON SslConfiguration

-- | Contains the information required to retrieve an app or cookbook from a
-- repository. For more information, see Creating Apps or Custom Recipes and
-- Cookbooks.
data Source = Source
    { sPassword :: Maybe Text
      -- ^ This parameter depends on the repository type. For Amazon S3 bundles, set
      -- Password to the appropriate AWS secret key. For HTTP bundles, Git
      -- repositories, and Subversion repositories, set Password to the password.
    , sRevision :: Maybe Text
      -- ^ The application's version. AWS OpsWorks enables you to easily deploy new
      -- versions of an application. One of the simplest approaches is to have
      -- branches or revisions in your repository that represent different versions
      -- that can potentially be deployed.
    , sSshKey :: Maybe Text
      -- ^ The repository's SSH key.
    , sType :: Maybe SourceType
      -- ^ The repository type.
    , sUrl :: Maybe Text
      -- ^ The source URL.
    , sUsername :: Maybe Text
      -- ^ This parameter depends on the repository type. For Amazon S3 bundles, set
      -- Username to the appropriate AWS access key. For HTTP bundles, Git
      -- repositories, and Subversion repositories, set Username to the user name.
    } deriving (Eq, Show, Generic)

instance FromJSON Source
instance ToJSON Source

-- | Describes an AWS OpsWorks service error.
data ServiceError = ServiceError
    { seCreatedAt :: Maybe Text
      -- ^ When the error occurred.
    , seInstanceId :: Maybe Text
      -- ^ The instance ID.
    , seMessage :: Maybe Text
      -- ^ A message that describes the error.
    , seServiceErrorId :: Maybe Text
      -- ^ The error ID.
    , seStackId :: Maybe Text
      -- ^ The stack ID.
    , seType :: Maybe Text
      -- ^ The error type.
    } deriving (Eq, Show, Generic)

instance FromJSON ServiceError
instance ToJSON ServiceError

-- | A UserProfile object that describes the user's SSH information.
data SelfUserProfile = SelfUserProfile
    { supIamUserArn :: Maybe Text
      -- ^ The user's IAM ARN.
    , supName :: Maybe Text
      -- ^ The user's name.
    , supSshPublicKey :: Maybe Text
      -- ^ The user's SSH public key.
    , supSshUsername :: Maybe Text
      -- ^ The user's SSH user name.
    } deriving (Eq, Show, Generic)

instance FromJSON SelfUserProfile
instance ToJSON SelfUserProfile

-- | A LayerCustomRecipes object that specifies the layer custom recipes.
data Recipes = Recipes
    { rConfigure :: [Text]
      -- ^ An array of custom recipe names to be run following a configure event.
    , rDeploy :: [Text]
      -- ^ An array of custom recipe names to be run following a deploy event.
    , rSetup :: [Text]
      -- ^ An array of custom recipe names to be run following a setup event.
    , rShutdown :: [Text]
      -- ^ An array of custom recipe names to be run following a shutdown event.
    , rUndeploy :: [Text]
      -- ^ An array of custom recipe names to be run following a undeploy event.
    } deriving (Eq, Show, Generic)

instance FromJSON Recipes
instance ToJSON Recipes

-- | Describes an instance's RAID array.
data RaidArray = RaidArray
    { raAvailabilityZone :: Maybe Text
      -- ^ The array's Availability Zone. For more information, see Regions and
      -- Endpoints.
    , raCreatedAt :: Maybe Text
      -- ^ When the RAID array was created.
    , raDevice :: Maybe Text
      -- ^ The array's Linux device. For example /dev/mdadm0.
    , raInstanceId :: Maybe Text
      -- ^ The instance ID.
    , raMountPoint :: Maybe Text
      -- ^ The array's mount point.
    , raName :: Maybe Text
      -- ^ The array name.
    , raNumberOfDisks :: Maybe Int
      -- ^ The number of disks in the array.
    , raRaidArrayId :: Maybe Text
      -- ^ The array ID.
    , raRaidLevel :: Maybe Int
      -- ^ The RAID level.
    , raSize :: Maybe Int
      -- ^ The array's size.
    } deriving (Eq, Show, Generic)

instance FromJSON RaidArray
instance ToJSON RaidArray

-- | Describes stack or user permissions.
data Permission = Permission
    { pAllowSsh :: Maybe Bool
      -- ^ Whether the user can use SSH.
    , pAllowSudo :: Maybe Bool
      -- ^ Whether the user can use sudo.
    , pIamUserArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) for an AWS Identity and Access Management
      -- (IAM) role. For more information about IAM ARNs, see Using Identifiers.
    , pLevel :: Maybe Text
      -- ^ The user's permission level, which must be the following: deny show deploy
      -- manage iam_only For more information on the permissions associated with
      -- these levels, see Managing User Permissions.
    , pStackId :: Maybe Text
      -- ^ A stack ID.
    } deriving (Eq, Show, Generic)

instance FromJSON Permission
instance ToJSON Permission

-- | Describes a layer's load-based auto scaling configuration.
data LoadBasedAutoScalingConfiguration = LoadBasedAutoScalingConfiguration
    { lbascDownScaling :: Maybe AutoScalingThresholds
      -- ^ A LoadBasedAutoscalingInstruction object that describes the downscaling
      -- configuration, which defines how and when AWS OpsWorks reduces the number
      -- of instances.
    , lbascEnable :: Maybe Bool
      -- ^ Whether load-based auto scaling is enabled for the layer.
    , lbascLayerId :: Maybe Text
      -- ^ The layer ID.
    , lbascUpScaling :: Maybe AutoScalingThresholds
      -- ^ A LoadBasedAutoscalingInstruction object that describes the upscaling
      -- configuration, which defines how and when AWS OpsWorks increases the number
      -- of instances.
    } deriving (Eq, Show, Generic)

instance FromJSON LoadBasedAutoScalingConfiguration
instance ToJSON LoadBasedAutoScalingConfiguration

-- | Describes a layer.
data Layer = Layer
    { lAttributes :: HashMap LayerAttributesKeys Text
      -- ^ The layer attributes.
    , lAutoAssignElasticIps :: Maybe Bool
      -- ^ Whether to automatically assign an Elastic IP address to the layer's
      -- instances. For more information, see How to Edit a Layer.
    , lAutoAssignPublicIps :: Maybe Bool
      -- ^ For stacks that are running in a VPC, whether to automatically assign a
      -- public IP address to the layer's instances. For more information, see How
      -- to Edit a Layer.
    , lCreatedAt :: Maybe Text
      -- ^ Date when the layer was created.
    , lCustomInstanceProfileArn :: Maybe Text
      -- ^ The ARN of the default IAM profile to be used for the layer's EC2
      -- instances. For more information about IAM ARNs, see Using Identifiers.
    , lCustomRecipes :: Maybe Recipes
      -- ^ A LayerCustomRecipes object that specifies the layer's custom recipes.
    , lCustomSecurityGroupIds :: [Text]
      -- ^ An array containing the layer's custom security group IDs.
    , lDefaultRecipes :: Maybe Recipes
      -- ^ AWS OpsWorks supports five lifecycle events, setup, configuration, deploy,
      -- undeploy, and shutdown. For each layer, AWS OpsWorks runs a set of standard
      -- recipes for each event. In addition, you can provide custom recipes for any
      -- or all layers and events. AWS OpsWorks runs custom event recipes after the
      -- standard recipes. LayerCustomRecipes specifies the custom recipes for a
      -- particular layer to be run in response to each of the five events. To
      -- specify a recipe, use the cookbook's directory name in the repository
      -- followed by two colons and the recipe name, which is the recipe's file name
      -- without the .rb extension. For example: phpapp2::dbsetup specifies the
      -- dbsetup.rb recipe in the repository's phpapp2 folder.
    , lDefaultSecurityGroupNames :: [Text]
      -- ^ An array containing the layer's security group names.
    , lEnableAutoHealing :: Maybe Bool
      -- ^ Whether auto healing is disabled for the layer.
    , lInstallUpdatesOnBoot :: Maybe Bool
      -- ^ Whether to install operating system and package updates when the instance
      -- boots. The default value is true. If this value is set to false, you must
      -- then update your instances manually by using CreateDeployment to run the
      -- update_dependencies stack command or manually running yum (Amazon Linux) or
      -- apt-get (Ubuntu) on the instances. We strongly recommend using the default
      -- value of true, to ensure that your instances have the latest security
      -- updates.
    , lLayerId :: Maybe Text
      -- ^ The layer ID.
    , lName :: Maybe Text
      -- ^ The layer name.
    , lPackages :: [Text]
      -- ^ An array of Package objects that describe the layer's packages.
    , lShortname :: Maybe Text
      -- ^ The layer short name.
    , lStackId :: Maybe Text
      -- ^ The layer stack ID.
    , lType :: Maybe LayerType
      -- ^ The layer type, which must be one of the following: Custom
      -- GangliaMonitoringMaster HaProxy MemcachedServer MySqlMaster NodeJsAppServer
      -- PhpAppServer RailsAppServer WebServer.
    , lVolumeConfigurations :: [VolumeConfiguration]
      -- ^ A VolumeConfigurations object that describes the layer's Amazon EBS
      -- volumes.
    } deriving (Eq, Show, Generic)

instance FromJSON Layer
instance ToJSON Layer

-- | An InstancesCount object with the number of instances in each status.
data InstancesCount = InstancesCount
    { icBooting :: Maybe Int
      -- ^ The number of instances with booting status.
    , icConnectionLost :: Maybe Int
      -- ^ The number of instances with connection_lost status.
    , icPending :: Maybe Int
      -- ^ The number of instances with pending status.
    , icRebooting :: Maybe Int
      -- ^ The number of instances with rebooting status.
    , icRequested :: Maybe Int
      -- ^ The number of instances with requested status.
    , icRunningSetup :: Maybe Int
      -- ^ The number of instances with running_setup status.
    , icSetupFailed :: Maybe Int
      -- ^ The number of instances with setup_failed status.
    , icShuttingDown :: Maybe Int
      -- ^ The number of instances with shutting_down status.
    , icStartFailed :: Maybe Int
      -- ^ The number of instances with start_failed status.
    , icStopped :: Maybe Int
      -- ^ The number of instances with stopped status.
    , icTerminated :: Maybe Int
      -- ^ The number of instances with terminated status.
    , icTerminating :: Maybe Int
      -- ^ The number of instances with terminating status.
    } deriving (Eq, Show, Generic)

instance FromJSON InstancesCount
instance ToJSON InstancesCount

-- | Describes an instance.
data Instance = Instance
    { iAmiId :: Maybe Text
      -- ^ A custom AMI ID to be used to create the instance. The AMI should be based
      -- on one of the standard AWS OpsWorks APIs: Amazon Linux or Ubuntu 12.04 LTS.
      -- For more information, see Instances.
    , iArchitecture :: Maybe Architecture
      -- ^ The instance architecture, "i386" or "x86_64".
    , iAutoScalingType :: Maybe AutoScalingType
      -- ^ The instance's auto scaling type, which has three possible values:
      -- AlwaysRunning: A 24/7 instance, which is not affected by auto scaling.
      -- TimeBasedAutoScaling: A time-based auto scaling instance, which is started
      -- and stopped based on a specified schedule. LoadBasedAutoScaling: A
      -- load-based auto scaling instance, which is started and stopped based on
      -- load metrics.
    , iAvailabilityZone :: Maybe Text
      -- ^ The instance Availability Zone. For more information, see Regions and
      -- Endpoints.
    , iCreatedAt :: Maybe Text
      -- ^ The time that the instance was created.
    , iEc2InstanceId :: Maybe Text
      -- ^ The ID of the associated Amazon EC2 instance.
    , iElasticIp :: Maybe Text
      -- ^ The instance Elastic IP address .
    , iHostname :: Maybe Text
      -- ^ The instance host name.
    , iInstallUpdatesOnBoot :: Maybe Bool
      -- ^ Whether to install operating system and package updates when the instance
      -- boots. The default value is true. If this value is set to false, you must
      -- then update your instances manually by using CreateDeployment to run the
      -- update_dependencies stack command or manually running yum (Amazon Linux) or
      -- apt-get (Ubuntu) on the instances. We strongly recommend using the default
      -- value of true, to ensure that your instances have the latest security
      -- updates.
    , iInstanceId :: Maybe Text
      -- ^ The instance ID.
    , iInstanceProfileArn :: Maybe Text
      -- ^ The ARN of the instance's IAM profile. For more information about IAM ARNs,
      -- see Using Identifiers.
    , iInstanceType :: Maybe Text
      -- ^ The instance type. AWS OpsWorks supports all instance types except Cluster
      -- Compute, Cluster GPU, and High Memory Cluster. For more information, see
      -- Instance Families and Types. The parameter values that specify the various
      -- types are in the API Name column of the Available Instance Types table.
    , iLastServiceErrorId :: Maybe Text
      -- ^ The ID of the last service error. For more information, call
      -- DescribeServiceErrors.
    , iLayerIds :: [Text]
      -- ^ An array containing the instance layer IDs.
    , iOs :: Maybe Text
      -- ^ The instance operating system.
    , iPrivateDns :: Maybe Text
      -- ^ The instance private DNS name.
    , iPrivateIp :: Maybe Text
      -- ^ The instance private IP address.
    , iPublicDns :: Maybe Text
      -- ^ The instance public DNS name.
    , iPublicIp :: Maybe Text
      -- ^ The instance public IP address.
    , iRootDeviceType :: Maybe RootDeviceType
      -- ^ The instance root device type. For more information, see Storage for the
      -- Root Device.
    , iRootDeviceVolumeId :: Maybe Text
      -- ^ The root device volume ID.
    , iSecurityGroupIds :: [Text]
      -- ^ An array containing the instance security group IDs.
    , iSshHostDsaKeyFingerprint :: Maybe Text
      -- ^ The SSH key's DSA fingerprint.
    , iSshHostRsaKeyFingerprint :: Maybe Text
      -- ^ The SSH key's RSA fingerprint.
    , iSshKeyName :: Maybe Text
      -- ^ The instance SSH key name.
    , iStackId :: Maybe Text
      -- ^ The stack ID.
    , iStatus :: Maybe Text
      -- ^ The instance status: requested booting running_setup online setup_failed
      -- start_failed terminating terminated stopped connection_lost.
    , iSubnetId :: Maybe Text
      -- ^ The instance's subnet ID, if the stack is running in a VPC.
    } deriving (Eq, Show, Generic)

instance FromJSON Instance
instance ToJSON Instance

-- | Describes an Elastic Load Balancing instance.
data ElasticLoadBalancer = ElasticLoadBalancer
    { elbAvailabilityZones :: [Text]
      -- ^ A list of Availability Zones.
    , elbDnsName :: Maybe Text
      -- ^ The instance's public DNS name.
    , elbEc2InstanceIds :: [Text]
      -- ^ A list of the EC2 instances that the Elastic Load Balancing instance is
      -- managing traffic for.
    , elbElasticLoadBalancerName :: Maybe Text
      -- ^ The Elastic Load Balancing instance's name.
    , elbLayerId :: Maybe Text
      -- ^ The ID of the layer that the instance is attached to.
    , elbRegion :: Maybe Text
      -- ^ The instance's AWS region.
    , elbStackId :: Maybe Text
      -- ^ The ID of the stack that the instance is associated with.
    , elbSubnetIds :: [Text]
      -- ^ A list of subnet IDs, if the stack is running in a VPC.
    , elbVpcId :: Maybe Text
      -- ^ The VPC ID.
    } deriving (Eq, Show, Generic)

instance FromJSON ElasticLoadBalancer
instance ToJSON ElasticLoadBalancer

-- | Describes an Elastic IP address.
data ElasticIp = ElasticIp
    { eiDomain :: Maybe Text
      -- ^ The domain.
    , eiInstanceId :: Maybe Text
      -- ^ The ID of the instance that the address is attached to.
    , eiIp :: Maybe Text
      -- ^ The IP address.
    , eiName :: Maybe Text
      -- ^ The name.
    , eiRegion :: Maybe Text
      -- ^ The AWS region. For more information, see Regions and Endpoints.
    } deriving (Eq, Show, Generic)

instance FromJSON ElasticIp
instance ToJSON ElasticIp

-- | A DeploymentCommand object that specifies the deployment command and any
-- associated arguments.
data DeploymentCommand = DeploymentCommand
    { dcArgs :: HashMap Text [Text]
      -- ^ An array of command arguments. This parameter is currently used only to
      -- specify the list of recipes to be executed by the ExecuteRecipes command.
    , dcName :: !DeploymentCommandName
      -- ^ Specifies the deployment operation. You can specify only one command. For
      -- stacks, the available commands are: execute_recipes: Execute the recipes
      -- that are specified by the Args parameter. install_dependencies: Installs
      -- the stack's dependencies. update_custom_cookbooks: Update the stack's
      -- custom cookbooks. update_dependencies: Update the stack's dependencies. For
      -- apps, the available commands are: deploy: Deploy the app. rollback Roll the
      -- app back to the previous version. When you update an app, AWS OpsWorks
      -- stores the previous version, up to a maximum of five versions. You can use
      -- this command to roll an app back as many as four versions. start: Start the
      -- app's web or application server. stop: Stop the app's web or application
      -- server. restart: Restart the app's web or application server. undeploy:
      -- Undeploy the app.
    } deriving (Eq, Show, Generic)

instance FromJSON DeploymentCommand
instance ToJSON DeploymentCommand

-- | Describes a deployment of a stack or app.
data Deployment = Deployment
    { dAppId :: Maybe Text
      -- ^ The app ID.
    , dCommand :: Maybe DeploymentCommand
      -- ^ Used to specify a deployment operation.
    , dComment :: Maybe Text
      -- ^ A user-defined comment.
    , dCompletedAt :: Maybe Text
      -- ^ Date when the deployment completed.
    , dCreatedAt :: Maybe Text
      -- ^ Date when the deployment was created.
    , dCustomJson :: Maybe Text
      -- ^ A string that contains user-defined custom JSON. It is used to override the
      -- corresponding default stack configuration JSON values for stack. The string
      -- should be in the following format and must escape characters such as '"'.:
      -- "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more information on
      -- custom JSON, see Use Custom JSON to Modify the Stack Configuration JSON.
    , dDeploymentId :: Maybe Text
      -- ^ The deployment ID.
    , dDuration :: Maybe Int
      -- ^ The deployment duration.
    , dIamUserArn :: Maybe Text
      -- ^ The user's IAM ARN.
    , dInstanceIds :: [Text]
      -- ^ The IDs of the target instances.
    , dStackId :: Maybe Text
      -- ^ The stack ID.
    , dStatus :: Maybe Text
      -- ^ The deployment status: running successful failed.
    } deriving (Eq, Show, Generic)

instance FromJSON Deployment
instance ToJSON Deployment

-- | Describes a command.
data Command = Command
    { cAcknowledgedAt :: Maybe Text
      -- ^ Date and time when the command was acknowledged.
    , cCommandId :: Maybe Text
      -- ^ The command ID.
    , cCompletedAt :: Maybe Text
      -- ^ Date when the command completed.
    , cCreatedAt :: Maybe Text
      -- ^ Date and time when the command was run.
    , cDeploymentId :: Maybe Text
      -- ^ The command deployment ID.
    , cExitCode :: Maybe Int
      -- ^ The command exit code.
    , cInstanceId :: Maybe Text
      -- ^ The ID of the instance where the command was executed.
    , cLogUrl :: Maybe Text
      -- ^ The URL of the command log.
    , cStatus :: Maybe Text
      -- ^ The command status: failed successful skipped pending.
    , cType :: Maybe Text
      -- ^ The command type: deploy rollback start stop restart undeploy
      -- update_dependencies install_dependencies update_custom_cookbooks
      -- execute_recipes.
    } deriving (Eq, Show, Generic)

instance FromJSON Command
instance ToJSON Command

-- | An AutoScalingThresholds object with the upscaling threshold configuration.
-- If the load exceeds these thresholds for a specified amount of time, AWS
-- OpsWorks starts a specified number of instances.
data AutoScalingThresholds = AutoScalingThresholds
    { astCpuThreshold :: Maybe Double
      -- ^ The CPU utilization threshold, as a percent of the available CPU.
    , astIgnoreMetricsTime :: Maybe Int
      -- ^ The amount of time (in minutes) after a scaling event occurs that AWS
      -- OpsWorks should ignore metrics and not raise any additional scaling events.
      -- For example, AWS OpsWorks adds new instances following an upscaling event
      -- but the instances won't start reducing the load until they have been booted
      -- and configured. There is no point in raising additional scaling events
      -- during that operation, which typically takes several minutes.
      -- IgnoreMetricsTime allows you to direct AWS OpsWorks to not raise any
      -- scaling events long enough to get the new instances online.
    , astInstanceCount :: Maybe Int
      -- ^ The number of instances to add or remove when the load exceeds a threshold.
    , astLoadThreshold :: Maybe Double
      -- ^ The load threshold. For more information about how load is computed, see
      -- Load (computing).
    , astMemoryThreshold :: Maybe Double
      -- ^ The memory utilization threshold, as a percent of the available memory.
    , astThresholdsWaitTime :: Maybe Int
      -- ^ The amount of time, in minutes, that the load must exceed a threshold
      -- before more instances are added or removed.
    } deriving (Eq, Show, Generic)

instance FromJSON AutoScalingThresholds
instance ToJSON AutoScalingThresholds

-- | A description of the app.
data App = App
    { aAppId :: Maybe Text
      -- ^ The app ID.
    , aAppSource :: Maybe Source
      -- ^ A Source object that describes the app repository.
    , aAttributes :: HashMap AppAttributesKeys Text
      -- ^ The contents of the stack attributes bag.
    , aCreatedAt :: Maybe Text
      -- ^ When the app was created.
    , aDescription :: Maybe Text
      -- ^ A description of the app.
    , aDomains :: [Text]
      -- ^ The app vhost settings, with multiple domains separated by commas. For
      -- example: 'www.example.com, example.com'.
    , aEnableSsl :: Maybe Bool
      -- ^ Whether to enable SSL for the app.
    , aName :: Maybe Text
      -- ^ The app name.
    , aShortname :: Maybe Text
      -- ^ The app's short name.
    , aSslConfiguration :: Maybe SslConfiguration
      -- ^ An SslConfiguration object with the SSL configuration.
    , aStackId :: Maybe Text
      -- ^ The app stack ID.
    , aType :: Maybe AppType
      -- ^ The app type.
    } deriving (Eq, Show, Generic)

instance FromJSON App
instance ToJSON App

-- | FIXME: Type documentation for StackAttributesKeys

data StackAttributesKeys
    = StackAttributesKeysColor
      deriving (Eq, Ord, Generic)

instance Hashable StackAttributesKeys

instance FromText StackAttributesKeys where
    fromText "Color" = Right StackAttributesKeysColor
    fromText e = fromTextFail $ "Unrecognised StackAttributesKeys: " <> e

instance Read StackAttributesKeys where
    readsPrec _ = fromTextRead

instance ToText StackAttributesKeys where
    toText StackAttributesKeysColor = "Color"

instance Show StackAttributesKeys where
    show = toTextShow

instance FromJSON StackAttributesKeys where
    parseJSON = fromTextJSON "StackAttributesKeys"

instance FromJSON v => FromJSON (HashMap StackAttributesKeys v) where
    parseJSON = fromTextHashJSON

instance ToJSON StackAttributesKeys where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap StackAttributesKeys v) where
    toJSON = toTextHashJSON

-- | The repository type.

data SourceType
    = SourceTypeArchive
    | SourceTypeGit
    | SourceTypeS3
    | SourceTypeSvn
      deriving (Eq, Ord, Generic)

instance Hashable SourceType

instance FromText SourceType where
    fromText "archive" = Right SourceTypeArchive
    fromText "git" = Right SourceTypeGit
    fromText "s3" = Right SourceTypeS3
    fromText "svn" = Right SourceTypeSvn
    fromText e = fromTextFail $ "Unrecognised SourceType: " <> e

instance Read SourceType where
    readsPrec _ = fromTextRead

instance ToText SourceType where
    toText SourceTypeArchive = "archive"
    toText SourceTypeGit = "git"
    toText SourceTypeS3 = "s3"
    toText SourceTypeSvn = "svn"

instance Show SourceType where
    show = toTextShow

instance FromJSON SourceType where
    parseJSON = fromTextJSON "SourceType"

instance FromJSON v => FromJSON (HashMap SourceType v) where
    parseJSON = fromTextHashJSON

instance ToJSON SourceType where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap SourceType v) where
    toJSON = toTextHashJSON

-- | The default root device type. This value is used by default for all
-- instances in the cloned stack, but you can override it when you create an
-- instance. For more information, see Storage for the Root Device.

data RootDeviceType
    = RootDeviceTypeEbs
    | RootDeviceTypeInstanceStore
      deriving (Eq, Ord, Generic)

instance Hashable RootDeviceType

instance FromText RootDeviceType where
    fromText "ebs" = Right RootDeviceTypeEbs
    fromText "instance-store" = Right RootDeviceTypeInstanceStore
    fromText e = fromTextFail $ "Unrecognised RootDeviceType: " <> e

instance Read RootDeviceType where
    readsPrec _ = fromTextRead

instance ToText RootDeviceType where
    toText RootDeviceTypeEbs = "ebs"
    toText RootDeviceTypeInstanceStore = "instance-store"

instance Show RootDeviceType where
    show = toTextShow

instance FromJSON RootDeviceType where
    parseJSON = fromTextJSON "RootDeviceType"

instance FromJSON v => FromJSON (HashMap RootDeviceType v) where
    parseJSON = fromTextHashJSON

instance ToJSON RootDeviceType where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap RootDeviceType v) where
    toJSON = toTextHashJSON

-- | The layer type. A stack cannot have more than one layer of the same type.
-- This parameter must be set to one of the following: lb: An HAProxy layer
-- web: A Static Web Server layer rails-app: A Rails App Server layer php-app:
-- A PHP App Server layer nodejs-app: A Node.js App Server layer memcached: A
-- Memcached layer db-master: A MySQL layer monitoring-master: A Ganglia layer
-- custom: A custom layer.

data LayerType
    = LayerTypeCustom
    | LayerTypeDbMaster
    | LayerTypeLb
    | LayerTypeMemcached
    | LayerTypeMonitoringMaster
    | LayerTypeNodejsApp
    | LayerTypePhpApp
    | LayerTypeRailsApp
    | LayerTypeWeb
      deriving (Eq, Ord, Generic)

instance Hashable LayerType

instance FromText LayerType where
    fromText "custom" = Right LayerTypeCustom
    fromText "db-master" = Right LayerTypeDbMaster
    fromText "lb" = Right LayerTypeLb
    fromText "memcached" = Right LayerTypeMemcached
    fromText "monitoring-master" = Right LayerTypeMonitoringMaster
    fromText "nodejs-app" = Right LayerTypeNodejsApp
    fromText "php-app" = Right LayerTypePhpApp
    fromText "rails-app" = Right LayerTypeRailsApp
    fromText "web" = Right LayerTypeWeb
    fromText e = fromTextFail $ "Unrecognised LayerType: " <> e

instance Read LayerType where
    readsPrec _ = fromTextRead

instance ToText LayerType where
    toText LayerTypeCustom = "custom"
    toText LayerTypeDbMaster = "db-master"
    toText LayerTypeLb = "lb"
    toText LayerTypeMemcached = "memcached"
    toText LayerTypeMonitoringMaster = "monitoring-master"
    toText LayerTypeNodejsApp = "nodejs-app"
    toText LayerTypePhpApp = "php-app"
    toText LayerTypeRailsApp = "rails-app"
    toText LayerTypeWeb = "web"

instance Show LayerType where
    show = toTextShow

instance FromJSON LayerType where
    parseJSON = fromTextJSON "LayerType"

instance FromJSON v => FromJSON (HashMap LayerType v) where
    parseJSON = fromTextHashJSON

instance ToJSON LayerType where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap LayerType v) where
    toJSON = toTextHashJSON

-- | FIXME: Type documentation for LayerAttributesKeys

data LayerAttributesKeys
    = LayerAttributesKeysBundlerVersion
    | LayerAttributesKeysEnableHaproxyStats
    | LayerAttributesKeysGangliaPassword
    | LayerAttributesKeysGangliaUrl
    | LayerAttributesKeysGangliaUser
    | LayerAttributesKeysHaproxyHealthCheckMethod
    | LayerAttributesKeysHaproxyHealthCheckUrl
    | LayerAttributesKeysHaproxyStatsPassword
    | LayerAttributesKeysHaproxyStatsUrl
    | LayerAttributesKeysHaproxyStatsUser
    | LayerAttributesKeysJavaAppServer
    | LayerAttributesKeysJavaAppServerVersion
    | LayerAttributesKeysJvm
    | LayerAttributesKeysJvmOptions
    | LayerAttributesKeysJvmVersion
    | LayerAttributesKeysManageBundler
    | LayerAttributesKeysMemcachedMemory
    | LayerAttributesKeysMysqlRootPassword
    | LayerAttributesKeysMysqlRootPasswordUbiquitous
    | LayerAttributesKeysNodejsVersion
    | LayerAttributesKeysPassengerVersion
    | LayerAttributesKeysRailsStack
    | LayerAttributesKeysRubyVersion
    | LayerAttributesKeysRubygemsVersion
      deriving (Eq, Ord, Generic)

instance Hashable LayerAttributesKeys

instance FromText LayerAttributesKeys where
    fromText "BundlerVersion" = Right LayerAttributesKeysBundlerVersion
    fromText "EnableHaproxyStats" = Right LayerAttributesKeysEnableHaproxyStats
    fromText "GangliaPassword" = Right LayerAttributesKeysGangliaPassword
    fromText "GangliaUrl" = Right LayerAttributesKeysGangliaUrl
    fromText "GangliaUser" = Right LayerAttributesKeysGangliaUser
    fromText "HaproxyHealthCheckMethod" = Right LayerAttributesKeysHaproxyHealthCheckMethod
    fromText "HaproxyHealthCheckUrl" = Right LayerAttributesKeysHaproxyHealthCheckUrl
    fromText "HaproxyStatsPassword" = Right LayerAttributesKeysHaproxyStatsPassword
    fromText "HaproxyStatsUrl" = Right LayerAttributesKeysHaproxyStatsUrl
    fromText "HaproxyStatsUser" = Right LayerAttributesKeysHaproxyStatsUser
    fromText "JavaAppServer" = Right LayerAttributesKeysJavaAppServer
    fromText "JavaAppServerVersion" = Right LayerAttributesKeysJavaAppServerVersion
    fromText "Jvm" = Right LayerAttributesKeysJvm
    fromText "JvmOptions" = Right LayerAttributesKeysJvmOptions
    fromText "JvmVersion" = Right LayerAttributesKeysJvmVersion
    fromText "ManageBundler" = Right LayerAttributesKeysManageBundler
    fromText "MemcachedMemory" = Right LayerAttributesKeysMemcachedMemory
    fromText "MysqlRootPassword" = Right LayerAttributesKeysMysqlRootPassword
    fromText "MysqlRootPasswordUbiquitous" = Right LayerAttributesKeysMysqlRootPasswordUbiquitous
    fromText "NodejsVersion" = Right LayerAttributesKeysNodejsVersion
    fromText "PassengerVersion" = Right LayerAttributesKeysPassengerVersion
    fromText "RailsStack" = Right LayerAttributesKeysRailsStack
    fromText "RubyVersion" = Right LayerAttributesKeysRubyVersion
    fromText "RubygemsVersion" = Right LayerAttributesKeysRubygemsVersion
    fromText e = fromTextFail $ "Unrecognised LayerAttributesKeys: " <> e

instance Read LayerAttributesKeys where
    readsPrec _ = fromTextRead

instance ToText LayerAttributesKeys where
    toText LayerAttributesKeysBundlerVersion = "BundlerVersion"
    toText LayerAttributesKeysEnableHaproxyStats = "EnableHaproxyStats"
    toText LayerAttributesKeysGangliaPassword = "GangliaPassword"
    toText LayerAttributesKeysGangliaUrl = "GangliaUrl"
    toText LayerAttributesKeysGangliaUser = "GangliaUser"
    toText LayerAttributesKeysHaproxyHealthCheckMethod = "HaproxyHealthCheckMethod"
    toText LayerAttributesKeysHaproxyHealthCheckUrl = "HaproxyHealthCheckUrl"
    toText LayerAttributesKeysHaproxyStatsPassword = "HaproxyStatsPassword"
    toText LayerAttributesKeysHaproxyStatsUrl = "HaproxyStatsUrl"
    toText LayerAttributesKeysHaproxyStatsUser = "HaproxyStatsUser"
    toText LayerAttributesKeysJavaAppServer = "JavaAppServer"
    toText LayerAttributesKeysJavaAppServerVersion = "JavaAppServerVersion"
    toText LayerAttributesKeysJvm = "Jvm"
    toText LayerAttributesKeysJvmOptions = "JvmOptions"
    toText LayerAttributesKeysJvmVersion = "JvmVersion"
    toText LayerAttributesKeysManageBundler = "ManageBundler"
    toText LayerAttributesKeysMemcachedMemory = "MemcachedMemory"
    toText LayerAttributesKeysMysqlRootPassword = "MysqlRootPassword"
    toText LayerAttributesKeysMysqlRootPasswordUbiquitous = "MysqlRootPasswordUbiquitous"
    toText LayerAttributesKeysNodejsVersion = "NodejsVersion"
    toText LayerAttributesKeysPassengerVersion = "PassengerVersion"
    toText LayerAttributesKeysRailsStack = "RailsStack"
    toText LayerAttributesKeysRubyVersion = "RubyVersion"
    toText LayerAttributesKeysRubygemsVersion = "RubygemsVersion"

instance Show LayerAttributesKeys where
    show = toTextShow

instance FromJSON LayerAttributesKeys where
    parseJSON = fromTextJSON "LayerAttributesKeys"

instance FromJSON v => FromJSON (HashMap LayerAttributesKeys v) where
    parseJSON = fromTextHashJSON

instance ToJSON LayerAttributesKeys where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap LayerAttributesKeys v) where
    toJSON = toTextHashJSON

-- | Specifies the deployment operation. You can specify only one command. For
-- stacks, the available commands are: execute_recipes: Execute the recipes
-- that are specified by the Args parameter. install_dependencies: Installs
-- the stack's dependencies. update_custom_cookbooks: Update the stack's
-- custom cookbooks. update_dependencies: Update the stack's dependencies. For
-- apps, the available commands are: deploy: Deploy the app. rollback Roll the
-- app back to the previous version. When you update an app, AWS OpsWorks
-- stores the previous version, up to a maximum of five versions. You can use
-- this command to roll an app back as many as four versions. start: Start the
-- app's web or application server. stop: Stop the app's web or application
-- server. restart: Restart the app's web or application server. undeploy:
-- Undeploy the app.

data DeploymentCommandName
    = DeploymentCommandNameDeploy
    | DeploymentCommandNameExecute_Recipes
    | DeploymentCommandNameInstall_Dependencies
    | DeploymentCommandNameRestart
    | DeploymentCommandNameRollback
    | DeploymentCommandNameStart
    | DeploymentCommandNameStop
    | DeploymentCommandNameUndeploy
    | DeploymentCommandNameUpdate_Custom_Cookbooks
    | DeploymentCommandNameUpdate_Dependencies
      deriving (Eq, Ord, Generic)

instance Hashable DeploymentCommandName

instance FromText DeploymentCommandName where
    fromText "deploy" = Right DeploymentCommandNameDeploy
    fromText "execute_recipes" = Right DeploymentCommandNameExecute_Recipes
    fromText "install_dependencies" = Right DeploymentCommandNameInstall_Dependencies
    fromText "restart" = Right DeploymentCommandNameRestart
    fromText "rollback" = Right DeploymentCommandNameRollback
    fromText "start" = Right DeploymentCommandNameStart
    fromText "stop" = Right DeploymentCommandNameStop
    fromText "undeploy" = Right DeploymentCommandNameUndeploy
    fromText "update_custom_cookbooks" = Right DeploymentCommandNameUpdate_Custom_Cookbooks
    fromText "update_dependencies" = Right DeploymentCommandNameUpdate_Dependencies
    fromText e = fromTextFail $ "Unrecognised DeploymentCommandName: " <> e

instance Read DeploymentCommandName where
    readsPrec _ = fromTextRead

instance ToText DeploymentCommandName where
    toText DeploymentCommandNameDeploy = "deploy"
    toText DeploymentCommandNameExecute_Recipes = "execute_recipes"
    toText DeploymentCommandNameInstall_Dependencies = "install_dependencies"
    toText DeploymentCommandNameRestart = "restart"
    toText DeploymentCommandNameRollback = "rollback"
    toText DeploymentCommandNameStart = "start"
    toText DeploymentCommandNameStop = "stop"
    toText DeploymentCommandNameUndeploy = "undeploy"
    toText DeploymentCommandNameUpdate_Custom_Cookbooks = "update_custom_cookbooks"
    toText DeploymentCommandNameUpdate_Dependencies = "update_dependencies"

instance Show DeploymentCommandName where
    show = toTextShow

instance FromJSON DeploymentCommandName where
    parseJSON = fromTextJSON "DeploymentCommandName"

instance FromJSON v => FromJSON (HashMap DeploymentCommandName v) where
    parseJSON = fromTextHashJSON

instance ToJSON DeploymentCommandName where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap DeploymentCommandName v) where
    toJSON = toTextHashJSON

-- | The instance auto scaling type, which has three possible values:
-- AlwaysRunning: A 24/7 instance, which is not affected by auto scaling.
-- TimeBasedAutoScaling: A time-based auto scaling instance, which is started
-- and stopped based on a specified schedule. To specify the schedule, call
-- SetTimeBasedAutoScaling. LoadBasedAutoScaling: A load-based auto scaling
-- instance, which is started and stopped based on load metrics. To use
-- load-based auto scaling, you must enable it for the instance layer and
-- configure the thresholds by calling SetLoadBasedAutoScaling.

data AutoScalingType
    = AutoScalingTypeLoad
    | AutoScalingTypeTimer
      deriving (Eq, Ord, Generic)

instance Hashable AutoScalingType

instance FromText AutoScalingType where
    fromText "load" = Right AutoScalingTypeLoad
    fromText "timer" = Right AutoScalingTypeTimer
    fromText e = fromTextFail $ "Unrecognised AutoScalingType: " <> e

instance Read AutoScalingType where
    readsPrec _ = fromTextRead

instance ToText AutoScalingType where
    toText AutoScalingTypeLoad = "load"
    toText AutoScalingTypeTimer = "timer"

instance Show AutoScalingType where
    show = toTextShow

instance FromJSON AutoScalingType where
    parseJSON = fromTextJSON "AutoScalingType"

instance FromJSON v => FromJSON (HashMap AutoScalingType v) where
    parseJSON = fromTextHashJSON

instance ToJSON AutoScalingType where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap AutoScalingType v) where
    toJSON = toTextHashJSON

-- | The instance architecture. Instance types do not necessarily support both
-- architectures. For a list of the architectures that are supported by the
-- different instance types, see Instance Families and Types.

data Architecture
    = ArchitectureI386
    | ArchitectureX86_64
      deriving (Eq, Ord, Generic)

instance Hashable Architecture

instance FromText Architecture where
    fromText "i386" = Right ArchitectureI386
    fromText "x86_64" = Right ArchitectureX86_64
    fromText e = fromTextFail $ "Unrecognised Architecture: " <> e

instance Read Architecture where
    readsPrec _ = fromTextRead

instance ToText Architecture where
    toText ArchitectureI386 = "i386"
    toText ArchitectureX86_64 = "x86_64"

instance Show Architecture where
    show = toTextShow

instance FromJSON Architecture where
    parseJSON = fromTextJSON "Architecture"

instance FromJSON v => FromJSON (HashMap Architecture v) where
    parseJSON = fromTextHashJSON

instance ToJSON Architecture where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap Architecture v) where
    toJSON = toTextHashJSON

-- | The app type.

data AppType
    = AppTypeNodejs
    | AppTypeOther
    | AppTypePhp
    | AppTypeRails
    | AppTypeStatic
      deriving (Eq, Ord, Generic)

instance Hashable AppType

instance FromText AppType where
    fromText "nodejs" = Right AppTypeNodejs
    fromText "other" = Right AppTypeOther
    fromText "php" = Right AppTypePhp
    fromText "rails" = Right AppTypeRails
    fromText "static" = Right AppTypeStatic
    fromText e = fromTextFail $ "Unrecognised AppType: " <> e

instance Read AppType where
    readsPrec _ = fromTextRead

instance ToText AppType where
    toText AppTypeNodejs = "nodejs"
    toText AppTypeOther = "other"
    toText AppTypePhp = "php"
    toText AppTypeRails = "rails"
    toText AppTypeStatic = "static"

instance Show AppType where
    show = toTextShow

instance FromJSON AppType where
    parseJSON = fromTextJSON "AppType"

instance FromJSON v => FromJSON (HashMap AppType v) where
    parseJSON = fromTextHashJSON

instance ToJSON AppType where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap AppType v) where
    toJSON = toTextHashJSON

-- | FIXME: Type documentation for AppAttributesKeys

data AppAttributesKeys
    = AppAttributesKeysAutoBundleOnDeploy
    | AppAttributesKeysDocumentRoot
    | AppAttributesKeysRailsEnv
      deriving (Eq, Ord, Generic)

instance Hashable AppAttributesKeys

instance FromText AppAttributesKeys where
    fromText "AutoBundleOnDeploy" = Right AppAttributesKeysAutoBundleOnDeploy
    fromText "DocumentRoot" = Right AppAttributesKeysDocumentRoot
    fromText "RailsEnv" = Right AppAttributesKeysRailsEnv
    fromText e = fromTextFail $ "Unrecognised AppAttributesKeys: " <> e

instance Read AppAttributesKeys where
    readsPrec _ = fromTextRead

instance ToText AppAttributesKeys where
    toText AppAttributesKeysAutoBundleOnDeploy = "AutoBundleOnDeploy"
    toText AppAttributesKeysDocumentRoot = "DocumentRoot"
    toText AppAttributesKeysRailsEnv = "RailsEnv"

instance Show AppAttributesKeys where
    show = toTextShow

instance FromJSON AppAttributesKeys where
    parseJSON = fromTextJSON "AppAttributesKeys"

instance FromJSON v => FromJSON (HashMap AppAttributesKeys v) where
    parseJSON = fromTextHashJSON

instance ToJSON AppAttributesKeys where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap AppAttributesKeys v) where
    toJSON = toTextHashJSON
