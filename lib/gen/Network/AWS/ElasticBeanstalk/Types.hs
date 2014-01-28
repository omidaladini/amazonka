{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.ElasticBeanstalk.Types where

import Data.ByteString      (ByteString)
import Data.Hashable
import Data.Monoid
import Data.List.NonEmpty
import Data.Text            (Text)
import Data.HashMap.Strict  (HashMap)
import Data.Time
import GHC.Generics         (Generic)
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.ElasticBeanstalk.Service

-- | An error or warning for a desired configuration option value.
data ValidationMessage = ValidationMessage
    { vmMessage :: Maybe Text
      -- ^ A message describing the error or warning.
    , vmNamespace :: Maybe Text
    , vmOptionName :: Maybe Text
    , vmSeverity :: Maybe ValidationSeverity
      -- ^ An indication of the severity of this message: error: This message
      -- indicates that this is not a valid setting for an option. warning: This
      -- message is providing information you should take into account. error: This
      -- message indicates that this is not a valid setting for an option. warning:
      -- This message is providing information you should take into account.
    } deriving (Eq, Show, Generic)

instance ToQuery ValidationMessage

instance FromXML ValidationMessage where
    fromXMLOptions = xmlOptions

instance ToXML ValidationMessage where
    toXMLOptions = xmlOptions

-- | Describes a trigger.
newtype Trigger = Trigger
    { tName :: Maybe Text
      -- ^ The name of the trigger.
    } deriving (Eq, Show, Generic)

instance ToQuery Trigger

instance FromXML Trigger where
    fromXMLOptions = xmlOptions

instance ToXML Trigger where
    toXMLOptions = xmlOptions

-- | If specified, AWS Elastic Beanstalk uses the configuration values from the
-- specified configuration template to create a new configuration. Values
-- specified in the OptionSettings parameter of this call overrides any values
-- obtained from the SourceConfiguration. If no configuration template is
-- found, returns an InvalidParameterValue error. Constraint: If both the
-- solution stack name parameter and the source configuration parameters are
-- specified, the solution stack of the source configuration template must
-- match the specified solution stack name or else AWS Elastic Beanstalk
-- returns an InvalidParameterCombination error.
data SourceConfiguration = SourceConfiguration
    { scApplicationName :: Maybe Text
      -- ^ The name of the application associated with the configuration.
    , scTemplateName :: Maybe Text
      -- ^ The name of the configuration template.
    } deriving (Eq, Show, Generic)

instance ToQuery SourceConfiguration

instance FromXML SourceConfiguration where
    fromXMLOptions = xmlOptions

instance ToXML SourceConfiguration where
    toXMLOptions = xmlOptions

-- | Describes the solution stack.
data SolutionStackDescription = SolutionStackDescription
    { ssdPermittedFileTypes :: [Text]
      -- ^ The permitted file types allowed for a solution stack.
    , ssdSolutionStackName :: Maybe Text
      -- ^ The name of the solution stack.
    } deriving (Eq, Show, Generic)

instance ToQuery SolutionStackDescription

instance FromXML SolutionStackDescription where
    fromXMLOptions = xmlOptions

instance ToXML SolutionStackDescription where
    toXMLOptions = xmlOptions

-- | The Amazon S3 bucket and key that identify the location of the source
-- bundle for this version. If data found at the Amazon S3 location exceeds
-- the maximum allowed source bundle size, AWS Elastic Beanstalk returns an
-- InvalidParameterValue error. The maximum size allowed is 512 MB. Default:
-- If not specified, AWS Elastic Beanstalk uses a sample application. If only
-- partially specified (for example, a bucket is provided but not the key) or
-- if no data is found at the Amazon S3 location, AWS Elastic Beanstalk
-- returns an InvalidParameterCombination error.
data S3Location = S3Location
    { s3lS3Bucket :: Maybe Text
      -- ^ The Amazon S3 bucket where the data is located.
    , s3lS3Key :: Maybe Text
      -- ^ The Amazon S3 key where the data is located.
    } deriving (Eq, Show, Generic)

instance ToQuery S3Location

instance FromXML S3Location where
    fromXMLOptions = xmlOptions

instance ToXML S3Location where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for Queue
data Queue = Queue
    { qName :: Maybe Text
    , qURL :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery Queue

instance FromXML Queue where
    fromXMLOptions = xmlOptions

instance ToXML Queue where
    toXMLOptions = xmlOptions

-- | A specification identifying an individual configuration option.
data OptionSpecification = OptionSpecification
    { osNamespace :: Maybe Text
      -- ^ A unique namespace identifying the option's associated AWS resource.
    , osOptionName :: Maybe Text
      -- ^ The name of the configuration option.
    } deriving (Eq, Show, Generic)

instance ToQuery OptionSpecification

instance FromXML OptionSpecification where
    fromXMLOptions = xmlOptions

instance ToXML OptionSpecification where
    toXMLOptions = xmlOptions

-- | If specified, the configuration option must be a string value that
-- satisfies this regular expression.
data OptionRestrictionRegex = OptionRestrictionRegex
    { orrLabel :: Maybe Text
      -- ^ A unique name representing this regular expression.
    , orrPattern :: Maybe Text
      -- ^ The regular expression pattern that a string configuration option value
      -- with this restriction must match.
    } deriving (Eq, Show, Generic)

instance ToQuery OptionRestrictionRegex

instance FromXML OptionRestrictionRegex where
    fromXMLOptions = xmlOptions

instance ToXML OptionRestrictionRegex where
    toXMLOptions = xmlOptions

-- | Describes the LoadBalancer.
data LoadBalancerDescription = LoadBalancerDescription
    { lbdDomain :: Maybe Text
      -- ^ The domain name of the LoadBalancer.
    , lbdListeners :: [Listener]
      -- ^ A list of Listeners used by the LoadBalancer.
    , lbdLoadBalancerName :: Maybe Text
      -- ^ The name of the LoadBalancer.
    } deriving (Eq, Show, Generic)

instance ToQuery LoadBalancerDescription

instance FromXML LoadBalancerDescription where
    fromXMLOptions = xmlOptions

instance ToXML LoadBalancerDescription where
    toXMLOptions = xmlOptions

-- | Describes a LoadBalancer.
newtype LoadBalancer = LoadBalancer
    { lbName :: Maybe Text
      -- ^ The name of the LoadBalancer.
    } deriving (Eq, Show, Generic)

instance ToQuery LoadBalancer

instance FromXML LoadBalancer where
    fromXMLOptions = xmlOptions

instance ToXML LoadBalancer where
    toXMLOptions = xmlOptions

-- | Describes the properties of a Listener for the LoadBalancer.
data Listener = Listener
    { lPort :: Maybe Int
      -- ^ The port that is used by the Listener.
    , lProtocol :: Maybe Text
      -- ^ The protocol that is used by the Listener.
    } deriving (Eq, Show, Generic)

instance ToQuery Listener

instance FromXML Listener where
    fromXMLOptions = xmlOptions

instance ToXML Listener where
    toXMLOptions = xmlOptions

-- | Describes an Auto Scaling launch configuration.
newtype LaunchConfiguration = LaunchConfiguration
    { lcName :: Maybe Text
      -- ^ The name of the launch configuration.
    } deriving (Eq, Show, Generic)

instance ToQuery LaunchConfiguration

instance FromXML LaunchConfiguration where
    fromXMLOptions = xmlOptions

instance ToXML LaunchConfiguration where
    toXMLOptions = xmlOptions

-- | The description of an Amazon EC2 instance.
newtype Instance = Instance
    { iId :: Maybe Text
      -- ^ The ID of the Amazon EC2 instance.
    } deriving (Eq, Show, Generic)

instance ToQuery Instance

instance FromXML Instance where
    fromXMLOptions = xmlOptions

instance ToXML Instance where
    toXMLOptions = xmlOptions

-- | Describes an event.
data EventDescription = EventDescription
    { eeApplicationName :: Maybe Text
      -- ^ The application associated with the event.
    , eeEnvironmentName :: Maybe Text
      -- ^ The name of the environment associated with this event.
    , eeEventDate :: Maybe UTCTime
      -- ^ The date when the event occurred.
    , eeMessage :: Maybe Text
      -- ^ The event message.
    , eeRequestId :: Maybe Text
      -- ^ The web service request ID for the activity of this event.
    , eeSeverity :: Maybe EventSeverity
      -- ^ The severity level of this event.
    , eeTemplateName :: Maybe Text
      -- ^ The name of the configuration associated with this event.
    , eeVersionLabel :: Maybe Text
      -- ^ The release label for the application version associated with this event.
    } deriving (Eq, Show, Generic)

instance ToQuery EventDescription

instance FromXML EventDescription where
    fromXMLOptions = xmlOptions

instance ToXML EventDescription where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for EnvironmentTier
data EnvironmentTier = EnvironmentTier
    { etName :: Maybe Text
    , etType :: Maybe Text
    , etVersion :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery EnvironmentTier

instance FromXML EnvironmentTier where
    fromXMLOptions = xmlOptions

instance ToXML EnvironmentTier where
    toXMLOptions = xmlOptions

-- | The description of the AWS resources used by this environment.
newtype EnvironmentResourcesDescription = EnvironmentResourcesDescription
    { ereLoadBalancer :: Maybe LoadBalancerDescription
      -- ^ Describes the LoadBalancer.
    } deriving (Eq, Show, Generic)

instance ToQuery EnvironmentResourcesDescription

instance FromXML EnvironmentResourcesDescription where
    fromXMLOptions = xmlOptions

instance ToXML EnvironmentResourcesDescription where
    toXMLOptions = xmlOptions

-- | A list of EnvironmentResourceDescription.
data EnvironmentResourceDescription = EnvironmentResourceDescription
    { erdAutoScalingGroups :: [AutoScalingGroup]
      -- ^ The AutoScalingGroups used by this environment.
    , erdEnvironmentName :: Maybe Text
      -- ^ The name of the environment.
    , erdInstances :: [Instance]
      -- ^ The Amazon EC2 instances used by this environment.
    , erdLaunchConfigurations :: [LaunchConfiguration]
      -- ^ The Auto Scaling launch configurations in use by this environment.
    , erdLoadBalancers :: [LoadBalancer]
      -- ^ The LoadBalancers in use by this environment.
    , erdQueues :: [Queue]
    , erdTriggers :: [Trigger]
      -- ^ The AutoScaling triggers in use by this environment.
    } deriving (Eq, Show, Generic)

instance ToQuery EnvironmentResourceDescription

instance FromXML EnvironmentResourceDescription where
    fromXMLOptions = xmlOptions

instance ToXML EnvironmentResourceDescription where
    toXMLOptions = xmlOptions

-- | The information retrieved from the Amazon EC2 instances.
data EnvironmentInfoDescription = EnvironmentInfoDescription
    { eidEc2InstanceId :: Maybe Text
      -- ^ The Amazon EC2 Instance ID for this information.
    , eidInfoType :: Maybe EnvironmentInfoType
      -- ^ The type of information retrieved.
    , eidMessage :: Maybe Text
      -- ^ The retrieved information.
    , eidSampleTimestamp :: Maybe UTCTime
      -- ^ The time stamp when this information was retrieved.
    } deriving (Eq, Show, Generic)

instance ToQuery EnvironmentInfoDescription

instance FromXML EnvironmentInfoDescription where
    fromXMLOptions = xmlOptions

instance ToXML EnvironmentInfoDescription where
    toXMLOptions = xmlOptions

-- | Describes the properties of an environment.
data EnvironmentDescription = EnvironmentDescription
    { edApplicationName :: Maybe Text
      -- ^ The name of the application associated with this environment.
    , edCNAME :: Maybe Text
      -- ^ The URL to the CNAME for this environment.
    , edDateCreated :: Maybe UTCTime
      -- ^ The creation date for this environment.
    , edDateUpdated :: Maybe UTCTime
      -- ^ The last modified date for this environment.
    , edDescription :: Maybe Text
      -- ^ Describes this environment.
    , edEndpointURL :: Maybe Text
      -- ^ The URL to the LoadBalancer for this environment.
    , edEnvironmentId :: Maybe Text
      -- ^ The ID of this environment.
    , edEnvironmentName :: Maybe Text
      -- ^ The name of this environment.
    , edHealth :: Maybe EnvironmentHealth
      -- ^ Describes the health status of the environment. AWS Elastic Beanstalk
      -- indicates the failure levels for a running environment: Red : Indicates the
      -- environment is not working. Yellow: Indicates that something is wrong, the
      -- application might not be available, but the instances appear running.
      -- Green: Indicates the environment is healthy and fully functional. Red:
      -- Indicates the environment is not responsive. Occurs when three or more
      -- consecutive failures occur for an environment. Yellow: Indicates that
      -- something is wrong. Occurs when two consecutive failures occur for an
      -- environment. Green: Indicates the environment is healthy and fully
      -- functional. Grey: Default health for a new environment. The environment is
      -- not fully launched and health checks have not started or health checks are
      -- suspended during an UpdateEnvironment or RestartEnvironement request.
      -- Default: Grey.
    , edResources :: Maybe EnvironmentResourcesDescription
      -- ^ The description of the AWS resources used by this environment.
    , edSolutionStackName :: Maybe Text
      -- ^ The name of the SolutionStack deployed with this environment.
    , edStatus :: Maybe EnvironmentStatus
      -- ^ The current operational status of the environment: Launching: Environment
      -- is in the process of initial deployment. Updating: Environment is in the
      -- process of updating its configuration settings or application version.
      -- Ready: Environment is available to have an action performed on it, such as
      -- update or terminate. Terminating: Environment is in the shut-down process.
      -- Terminated: Environment is not running.
    , edTemplateName :: Maybe Text
      -- ^ The name of the configuration template used to originally launch this
      -- environment.
    , edTier :: Maybe EnvironmentTier
    , edVersionLabel :: Maybe Text
      -- ^ The application version deployed in this environment.
    } deriving (Eq, Show, Generic)

instance ToQuery EnvironmentDescription

instance FromXML EnvironmentDescription where
    fromXMLOptions = xmlOptions

instance ToXML EnvironmentDescription where
    toXMLOptions = xmlOptions

-- | Describes the settings for a configuration set.
data ConfigurationSettingsDescription = ConfigurationSettingsDescription
    { csdApplicationName :: Maybe Text
      -- ^ The name of the application associated with this configuration set.
    , csdDateCreated :: Maybe UTCTime
      -- ^ The date (in UTC time) when this configuration set was created.
    , csdDateUpdated :: Maybe UTCTime
      -- ^ The date (in UTC time) when this configuration set was last modified.
    , csdDeploymentStatus :: Maybe ConfigurationDeploymentStatus
      -- ^ If this configuration set is associated with an environment, the
      -- DeploymentStatus parameter indicates the deployment status of this
      -- configuration set: null: This configuration is not associated with a
      -- running environment. pending: This is a draft configuration that is not
      -- deployed to the associated environment but is in the process of deploying.
      -- deployed: This is the configuration that is currently deployed to the
      -- associated running environment. failed: This is a draft configuration, that
      -- failed to successfully deploy. null: This configuration is not associated
      -- with a running environment. pending: This is a draft configuration that is
      -- not deployed to the associated environment but is in the process of
      -- deploying. deployed: This is the configuration that is currently deployed
      -- to the associated running environment. failed: This is a draft
      -- configuration that failed to successfully deploy.
    , csdDescription :: Maybe Text
      -- ^ Describes this configuration set.
    , csdEnvironmentName :: Maybe Text
      -- ^ If not null, the name of the environment for this configuration set.
    , csdOptionSettings :: [ConfigurationOptionSetting]
      -- ^ A list of the configuration options and their values in this configuration
      -- set.
    , csdSolutionStackName :: Maybe Text
      -- ^ The name of the solution stack this configuration set uses.
    , csdTemplateName :: Maybe Text
      -- ^ If not null, the name of the configuration template for this configuration
      -- set.
    } deriving (Eq, Show, Generic)

instance ToQuery ConfigurationSettingsDescription

instance FromXML ConfigurationSettingsDescription where
    fromXMLOptions = xmlOptions

instance ToXML ConfigurationSettingsDescription where
    toXMLOptions = xmlOptions

-- | A specification identifying an individual configuration option along with
-- its current value. For a list of possible option values, go to Option
-- Values in the AWS Elastic Beanstalk Developer Guide.
data ConfigurationOptionSetting = ConfigurationOptionSetting
    { cosNamespace :: Maybe Text
      -- ^ A unique namespace identifying the option's associated AWS resource.
    , cosOptionName :: Maybe Text
      -- ^ The name of the configuration option.
    , cosValue :: Maybe Text
      -- ^ The current value for the configuration option.
    } deriving (Eq, Show, Generic)

instance ToQuery ConfigurationOptionSetting

instance FromXML ConfigurationOptionSetting where
    fromXMLOptions = xmlOptions

instance ToXML ConfigurationOptionSetting where
    toXMLOptions = xmlOptions

-- | Describes the possible values for a configuration option.
data ConfigurationOptionDescription = ConfigurationOptionDescription
    { codChangeSeverity :: Maybe Text
      -- ^ An indication of which action is required if the value for this
      -- configuration option changes: NoInterruption - There is no interruption to
      -- the environment or application availability. RestartEnvironment - The
      -- environment is restarted, all AWS resources are deleted and recreated, and
      -- the environment is unavailable during the process. RestartApplicationServer
      -- - The environment is available the entire time. However, a short
      -- application outage occurs when the application servers on the running
      -- Amazon EC2 instances are restarted. NoInterruption : There is no
      -- interruption to the environment or application availability.
      -- RestartEnvironment : The environment is entirely restarted, all AWS
      -- resources are deleted and recreated, and the environment is unavailable
      -- during the process. RestartApplicationServer : The environment is available
      -- the entire time. However, a short application outage occurs when the
      -- application servers on the running Amazon EC2 instances are restarted.
    , codDefaultValue :: Maybe Text
      -- ^ The default value for this configuration option.
    , codMaxLength :: Maybe Int
      -- ^ If specified, the configuration option must be a string value no longer
      -- than this value.
    , codMaxValue :: Maybe Int
      -- ^ If specified, the configuration option must be a numeric value less than
      -- this value.
    , codMinValue :: Maybe Int
      -- ^ If specified, the configuration option must be a numeric value greater than
      -- this value.
    , codName :: Maybe Text
      -- ^ The name of the configuration option.
    , codNamespace :: Maybe Text
      -- ^ A unique namespace identifying the option's associated AWS resource.
    , codRegex :: Maybe OptionRestrictionRegex
      -- ^ If specified, the configuration option must be a string value that
      -- satisfies this regular expression.
    , codUserDefined :: Maybe Bool
      -- ^ An indication of whether the user defined this configuration option: true :
      -- This configuration option was defined by the user. It is a valid choice for
      -- specifying this as an Option to Remove when updating configuration
      -- settings. false : This configuration was not defined by the user. true :
      -- This configuration option was defined by the user. It is a valid choice for
      -- specifying if this as an Option to Remove when updating configuration
      -- settings. false : This configuration was not defined by the user.
      -- Constraint: You can remove only UserDefined options from a configuration.
      -- Valid Values: true | false.
    , codValueOptions :: [Text]
      -- ^ If specified, values for the configuration option are selected from this
      -- list.
    , codValueType :: Maybe ConfigurationOptionValueType
      -- ^ An indication of which type of values this option has and whether it is
      -- allowable to select one or more than one of the possible values: Scalar :
      -- Values for this option are a single selection from the possible values, or
      -- a unformatted string or numeric value governed by the MIN/MAX/Regex
      -- constraints: List : Values for this option are multiple selections of the
      -- possible values. Boolean : Values for this option are either true or false
      -- . Scalar : Values for this option are a single selection from the possible
      -- values, or an unformatted string, or numeric value governed by the
      -- MIN/MAX/Regex constraints. List : Values for this option are multiple
      -- selections from the possible values. Boolean : Values for this option are
      -- either true or false .
    } deriving (Eq, Show, Generic)

instance ToQuery ConfigurationOptionDescription

instance FromXML ConfigurationOptionDescription where
    fromXMLOptions = xmlOptions

instance ToXML ConfigurationOptionDescription where
    toXMLOptions = xmlOptions

-- | Describes an Auto Scaling launch configuration.
newtype AutoScalingGroup = AutoScalingGroup
    { asgName :: Maybe Text
      -- ^ The name of the AutoScalingGroup .
    } deriving (Eq, Show, Generic)

instance ToQuery AutoScalingGroup

instance FromXML AutoScalingGroup where
    fromXMLOptions = xmlOptions

instance ToXML AutoScalingGroup where
    toXMLOptions = xmlOptions

-- | The ApplicationVersionDescription of the application version.
data ApplicationVersionDescription = ApplicationVersionDescription
    { avdApplicationName :: Maybe Text
      -- ^ The name of the application associated with this release.
    , avdDateCreated :: Maybe UTCTime
      -- ^ The creation date of the application version.
    , avdDateUpdated :: Maybe UTCTime
      -- ^ The last modified date of the application version.
    , avdDescription :: Maybe Text
      -- ^ The description of this application version.
    , avdSourceBundle :: Maybe S3Location
      -- ^ The location where the source bundle is located for this version.
    , avdVersionLabel :: Maybe Text
      -- ^ A label uniquely identifying the version for the associated application.
    } deriving (Eq, Show, Generic)

instance ToQuery ApplicationVersionDescription

instance FromXML ApplicationVersionDescription where
    fromXMLOptions = xmlOptions

instance ToXML ApplicationVersionDescription where
    toXMLOptions = xmlOptions

-- | Describes the properties of an application.
data ApplicationDescription = ApplicationDescription
    { adApplicationName :: Maybe Text
      -- ^ The name of the application.
    , adConfigurationTemplates :: [Text]
      -- ^ The names of the configuration templates associated with this application.
    , adDateCreated :: Maybe UTCTime
      -- ^ The date when the application was created.
    , adDateUpdated :: Maybe UTCTime
      -- ^ The date when the application was last modified.
    , adDescription :: Maybe Text
      -- ^ User-defined description of the application.
    , adVersions :: [Text]
      -- ^ The names of the versions for this application.
    } deriving (Eq, Show, Generic)

instance ToQuery ApplicationDescription

instance FromXML ApplicationDescription where
    fromXMLOptions = xmlOptions

instance ToXML ApplicationDescription where
    toXMLOptions = xmlOptions

-- | An indication of the severity of this message: error: This message
-- indicates that this is not a valid setting for an option. warning: This
-- message is providing information you should take into account. error: This
-- message indicates that this is not a valid setting for an option. warning:
-- This message is providing information you should take into account.
data ValidationSeverity
    = ValidationSeverityError
    | ValidationSeverityWarning
      deriving (Eq, Ord, Generic)

instance Hashable ValidationSeverity

instance FromText ValidationSeverity where
    fromText "error" = Right ValidationSeverityError
    fromText "warning" = Right ValidationSeverityWarning
    fromText e = fromTextFail $ "Unrecognised ValidationSeverity: " <> e

instance Read ValidationSeverity where
    readsPrec _ = fromTextRead

instance ToText ValidationSeverity where
    toText ValidationSeverityError = "error"
    toText ValidationSeverityWarning = "warning"

instance Show ValidationSeverity where
    show = toTextShow

instance ToQuery ValidationSeverity where
    toQuery = toTextQuery

instance FromXML ValidationSeverity where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ValidationSeverity where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | If specified, limits the events returned from this call to include only
-- those with the specified severity or higher.
data EventSeverity
    = EventSeverityDEBUG
    | EventSeverityERROR
    | EventSeverityFATAL
    | EventSeverityINFO
    | EventSeverityTRACE
    | EventSeverityWARN
      deriving (Eq, Ord, Generic)

instance Hashable EventSeverity

instance FromText EventSeverity where
    fromText "DEBUG" = Right EventSeverityDEBUG
    fromText "ERROR" = Right EventSeverityERROR
    fromText "FATAL" = Right EventSeverityFATAL
    fromText "INFO" = Right EventSeverityINFO
    fromText "TRACE" = Right EventSeverityTRACE
    fromText "WARN" = Right EventSeverityWARN
    fromText e = fromTextFail $ "Unrecognised EventSeverity: " <> e

instance Read EventSeverity where
    readsPrec _ = fromTextRead

instance ToText EventSeverity where
    toText EventSeverityDEBUG = "DEBUG"
    toText EventSeverityERROR = "ERROR"
    toText EventSeverityFATAL = "FATAL"
    toText EventSeverityINFO = "INFO"
    toText EventSeverityTRACE = "TRACE"
    toText EventSeverityWARN = "WARN"

instance Show EventSeverity where
    show = toTextShow

instance ToQuery EventSeverity where
    toQuery = toTextQuery

instance FromXML EventSeverity where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML EventSeverity where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The current operational status of the environment: Launching: Environment
-- is in the process of initial deployment. Updating: Environment is in the
-- process of updating its configuration settings or application version.
-- Ready: Environment is available to have an action performed on it, such as
-- update or terminate. Terminating: Environment is in the shut-down process.
-- Terminated: Environment is not running.
data EnvironmentStatus
    = EnvironmentStatusLaunching
    | EnvironmentStatusReady
    | EnvironmentStatusTerminated
    | EnvironmentStatusTerminating
    | EnvironmentStatusUpdating
      deriving (Eq, Ord, Generic)

instance Hashable EnvironmentStatus

instance FromText EnvironmentStatus where
    fromText "Launching" = Right EnvironmentStatusLaunching
    fromText "Ready" = Right EnvironmentStatusReady
    fromText "Terminated" = Right EnvironmentStatusTerminated
    fromText "Terminating" = Right EnvironmentStatusTerminating
    fromText "Updating" = Right EnvironmentStatusUpdating
    fromText e = fromTextFail $ "Unrecognised EnvironmentStatus: " <> e

instance Read EnvironmentStatus where
    readsPrec _ = fromTextRead

instance ToText EnvironmentStatus where
    toText EnvironmentStatusLaunching = "Launching"
    toText EnvironmentStatusReady = "Ready"
    toText EnvironmentStatusTerminated = "Terminated"
    toText EnvironmentStatusTerminating = "Terminating"
    toText EnvironmentStatusUpdating = "Updating"

instance Show EnvironmentStatus where
    show = toTextShow

instance ToQuery EnvironmentStatus where
    toQuery = toTextQuery

instance FromXML EnvironmentStatus where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML EnvironmentStatus where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The type of information to request.
data EnvironmentInfoType
    = EnvironmentInfoTypeTail
      deriving (Eq, Ord, Generic)

instance Hashable EnvironmentInfoType

instance FromText EnvironmentInfoType where
    fromText "tail" = Right EnvironmentInfoTypeTail
    fromText e = fromTextFail $ "Unrecognised EnvironmentInfoType: " <> e

instance Read EnvironmentInfoType where
    readsPrec _ = fromTextRead

instance ToText EnvironmentInfoType where
    toText EnvironmentInfoTypeTail = "tail"

instance Show EnvironmentInfoType where
    show = toTextShow

instance ToQuery EnvironmentInfoType where
    toQuery = toTextQuery

instance FromXML EnvironmentInfoType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML EnvironmentInfoType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | Describes the health status of the environment. AWS Elastic Beanstalk
-- indicates the failure levels for a running environment: Red : Indicates the
-- environment is not working. Yellow: Indicates that something is wrong, the
-- application might not be available, but the instances appear running.
-- Green: Indicates the environment is healthy and fully functional. Red:
-- Indicates the environment is not responsive. Occurs when three or more
-- consecutive failures occur for an environment. Yellow: Indicates that
-- something is wrong. Occurs when two consecutive failures occur for an
-- environment. Green: Indicates the environment is healthy and fully
-- functional. Grey: Default health for a new environment. The environment is
-- not fully launched and health checks have not started or health checks are
-- suspended during an UpdateEnvironment or RestartEnvironement request.
-- Default: Grey.
data EnvironmentHealth
    = EnvironmentHealthGreen
    | EnvironmentHealthGrey
    | EnvironmentHealthRed
    | EnvironmentHealthYellow
      deriving (Eq, Ord, Generic)

instance Hashable EnvironmentHealth

instance FromText EnvironmentHealth where
    fromText "Green" = Right EnvironmentHealthGreen
    fromText "Grey" = Right EnvironmentHealthGrey
    fromText "Red" = Right EnvironmentHealthRed
    fromText "Yellow" = Right EnvironmentHealthYellow
    fromText e = fromTextFail $ "Unrecognised EnvironmentHealth: " <> e

instance Read EnvironmentHealth where
    readsPrec _ = fromTextRead

instance ToText EnvironmentHealth where
    toText EnvironmentHealthGreen = "Green"
    toText EnvironmentHealthGrey = "Grey"
    toText EnvironmentHealthRed = "Red"
    toText EnvironmentHealthYellow = "Yellow"

instance Show EnvironmentHealth where
    show = toTextShow

instance ToQuery EnvironmentHealth where
    toQuery = toTextQuery

instance FromXML EnvironmentHealth where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML EnvironmentHealth where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | An indication of which type of values this option has and whether it is
-- allowable to select one or more than one of the possible values: Scalar :
-- Values for this option are a single selection from the possible values, or
-- a unformatted string or numeric value governed by the MIN/MAX/Regex
-- constraints: List : Values for this option are multiple selections of the
-- possible values. Boolean : Values for this option are either true or false
-- . Scalar : Values for this option are a single selection from the possible
-- values, or an unformatted string, or numeric value governed by the
-- MIN/MAX/Regex constraints. List : Values for this option are multiple
-- selections from the possible values. Boolean : Values for this option are
-- either true or false .
data ConfigurationOptionValueType
    = ConfigurationOptionValueTypeList
    | ConfigurationOptionValueTypeScalar
      deriving (Eq, Ord, Generic)

instance Hashable ConfigurationOptionValueType

instance FromText ConfigurationOptionValueType where
    fromText "List" = Right ConfigurationOptionValueTypeList
    fromText "Scalar" = Right ConfigurationOptionValueTypeScalar
    fromText e = fromTextFail $ "Unrecognised ConfigurationOptionValueType: " <> e

instance Read ConfigurationOptionValueType where
    readsPrec _ = fromTextRead

instance ToText ConfigurationOptionValueType where
    toText ConfigurationOptionValueTypeList = "List"
    toText ConfigurationOptionValueTypeScalar = "Scalar"

instance Show ConfigurationOptionValueType where
    show = toTextShow

instance ToQuery ConfigurationOptionValueType where
    toQuery = toTextQuery

instance FromXML ConfigurationOptionValueType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ConfigurationOptionValueType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | If this configuration set is associated with an environment, the
-- DeploymentStatus parameter indicates the deployment status of this
-- configuration set: null: This configuration is not associated with a
-- running environment. pending: This is a draft configuration that is not
-- deployed to the associated environment but is in the process of deploying.
-- deployed: This is the configuration that is currently deployed to the
-- associated running environment. failed: This is a draft configuration, that
-- failed to successfully deploy. null: This configuration is not associated
-- with a running environment. pending: This is a draft configuration that is
-- not deployed to the associated environment but is in the process of
-- deploying. deployed: This is the configuration that is currently deployed
-- to the associated running environment. failed: This is a draft
-- configuration that failed to successfully deploy.
data ConfigurationDeploymentStatus
    = ConfigurationDeploymentStatusDeployed
    | ConfigurationDeploymentStatusFailed
    | ConfigurationDeploymentStatusPending
      deriving (Eq, Ord, Generic)

instance Hashable ConfigurationDeploymentStatus

instance FromText ConfigurationDeploymentStatus where
    fromText "deployed" = Right ConfigurationDeploymentStatusDeployed
    fromText "failed" = Right ConfigurationDeploymentStatusFailed
    fromText "pending" = Right ConfigurationDeploymentStatusPending
    fromText e = fromTextFail $ "Unrecognised ConfigurationDeploymentStatus: " <> e

instance Read ConfigurationDeploymentStatus where
    readsPrec _ = fromTextRead

instance ToText ConfigurationDeploymentStatus where
    toText ConfigurationDeploymentStatusDeployed = "deployed"
    toText ConfigurationDeploymentStatusFailed = "failed"
    toText ConfigurationDeploymentStatusPending = "pending"

instance Show ConfigurationDeploymentStatus where
    show = toTextShow

instance ToQuery ConfigurationDeploymentStatus where
    toQuery = toTextQuery

instance FromXML ConfigurationDeploymentStatus where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ConfigurationDeploymentStatus where
    toXMLOptions = xmlOptions
    toXML        = toTextXML
