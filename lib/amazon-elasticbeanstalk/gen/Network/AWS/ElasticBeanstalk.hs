-- Module      : Network.AWS.ElasticBeanstalk
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.ElasticBeanstalk
    (
    -- * Operations
    -- ** DescribeApplications
      module Network.AWS.ElasticBeanstalk.DescribeApplications
    -- ** UpdateEnvironment
    , module Network.AWS.ElasticBeanstalk.UpdateEnvironment
    -- ** TerminateEnvironment
    , module Network.AWS.ElasticBeanstalk.TerminateEnvironment
    -- ** CreateApplicationVersion
    , module Network.AWS.ElasticBeanstalk.CreateApplicationVersion
    -- ** DescribeEvents
    , module Network.AWS.ElasticBeanstalk.DescribeEvents
    -- ** RequestEnvironmentInfo
    , module Network.AWS.ElasticBeanstalk.RequestEnvironmentInfo
    -- ** RetrieveEnvironmentInfo
    , module Network.AWS.ElasticBeanstalk.RetrieveEnvironmentInfo
    -- ** DeleteApplication
    , module Network.AWS.ElasticBeanstalk.DeleteApplication
    -- ** UpdateApplication
    , module Network.AWS.ElasticBeanstalk.UpdateApplication
    -- ** CreateApplication
    , module Network.AWS.ElasticBeanstalk.CreateApplication
    -- ** DeleteConfigurationTemplate
    , module Network.AWS.ElasticBeanstalk.DeleteConfigurationTemplate
    -- ** UpdateConfigurationTemplate
    , module Network.AWS.ElasticBeanstalk.UpdateConfigurationTemplate
    -- ** DescribeEnvironmentResources
    , module Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources
    -- ** DeleteApplicationVersion
    , module Network.AWS.ElasticBeanstalk.DeleteApplicationVersion
    -- ** UpdateApplicationVersion
    , module Network.AWS.ElasticBeanstalk.UpdateApplicationVersion
    -- ** CreateConfigurationTemplate
    , module Network.AWS.ElasticBeanstalk.CreateConfigurationTemplate
    -- ** RebuildEnvironment
    , module Network.AWS.ElasticBeanstalk.RebuildEnvironment
    -- ** DeleteEnvironmentConfiguration
    , module Network.AWS.ElasticBeanstalk.DeleteEnvironmentConfiguration
    -- ** SwapEnvironmentCNAMEs
    , module Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs
    -- ** ListAvailableSolutionStacks
    , module Network.AWS.ElasticBeanstalk.ListAvailableSolutionStacks
    -- ** DescribeConfigurationOptions
    , module Network.AWS.ElasticBeanstalk.DescribeConfigurationOptions
    -- ** CreateStorageLocation
    , module Network.AWS.ElasticBeanstalk.CreateStorageLocation
    -- ** DescribeConfigurationSettings
    , module Network.AWS.ElasticBeanstalk.DescribeConfigurationSettings
    -- ** ValidateConfigurationSettings
    , module Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings
    -- ** RestartAppServer
    , module Network.AWS.ElasticBeanstalk.RestartAppServer
    -- ** DescribeEnvironments
    , module Network.AWS.ElasticBeanstalk.DescribeEnvironments
    -- ** CheckDNSAvailability
    , module Network.AWS.ElasticBeanstalk.CheckDNSAvailability
    -- ** DescribeApplicationVersions
    , module Network.AWS.ElasticBeanstalk.DescribeApplicationVersions
    -- ** CreateEnvironment
    , module Network.AWS.ElasticBeanstalk.CreateEnvironment

    -- * Types
    -- ** ValidationMessage
    , ValidationMessage (..)
    -- ** Trigger
    , Trigger (..)
    -- ** SourceConfiguration
    , SourceConfiguration (..)
    -- ** SolutionStackDescription
    , SolutionStackDescription (..)
    -- ** S3Location
    , S3Location (..)
    -- ** Queue
    , Queue (..)
    -- ** OptionSpecification
    , OptionSpecification (..)
    -- ** OptionRestrictionRegex
    , OptionRestrictionRegex (..)
    -- ** LoadBalancerDescription
    , LoadBalancerDescription (..)
    -- ** LoadBalancer
    , LoadBalancer (..)
    -- ** Listener
    , Listener (..)
    -- ** LaunchConfiguration
    , LaunchConfiguration (..)
    -- ** Instance
    , Instance (..)
    -- ** EventDescription
    , EventDescription (..)
    -- ** EnvironmentTier
    , EnvironmentTier (..)
    -- ** EnvironmentResourcesDescription
    , EnvironmentResourcesDescription (..)
    -- ** EnvironmentResourceDescription
    , EnvironmentResourceDescription (..)
    -- ** EnvironmentInfoDescription
    , EnvironmentInfoDescription (..)
    -- ** EnvironmentDescription
    , EnvironmentDescription (..)
    -- ** ConfigurationSettingsDescription
    , ConfigurationSettingsDescription (..)
    -- ** ConfigurationOptionSetting
    , ConfigurationOptionSetting (..)
    -- ** ConfigurationOptionDescription
    , ConfigurationOptionDescription (..)
    -- ** AutoScalingGroup
    , AutoScalingGroup (..)
    -- ** ApplicationVersionDescription
    , ApplicationVersionDescription (..)
    -- ** ApplicationDescription
    , ApplicationDescription (..)
    -- ** ValidationSeverity
    , ValidationSeverity (..)
    -- ** EventSeverity
    , EventSeverity (..)
    -- ** EnvironmentStatus
    , EnvironmentStatus (..)
    -- ** EnvironmentInfoType
    , EnvironmentInfoType (..)
    -- ** EnvironmentHealth
    , EnvironmentHealth (..)
    -- ** ConfigurationOptionValueType
    , ConfigurationOptionValueType (..)
    -- ** ConfigurationDeploymentStatus
    , ConfigurationDeploymentStatus (..)

    -- * Errors
    , ElasticBeanstalkError (..)
    ) where

import Network.AWS.ElasticBeanstalk.Service
import Network.AWS.ElasticBeanstalk.Types

import Network.AWS.ElasticBeanstalk.DescribeApplications
import Network.AWS.ElasticBeanstalk.UpdateEnvironment
import Network.AWS.ElasticBeanstalk.TerminateEnvironment
import Network.AWS.ElasticBeanstalk.CreateApplicationVersion
import Network.AWS.ElasticBeanstalk.DescribeEvents
import Network.AWS.ElasticBeanstalk.RequestEnvironmentInfo
import Network.AWS.ElasticBeanstalk.RetrieveEnvironmentInfo
import Network.AWS.ElasticBeanstalk.DeleteApplication
import Network.AWS.ElasticBeanstalk.UpdateApplication
import Network.AWS.ElasticBeanstalk.CreateApplication
import Network.AWS.ElasticBeanstalk.DeleteConfigurationTemplate
import Network.AWS.ElasticBeanstalk.UpdateConfigurationTemplate
import Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources
import Network.AWS.ElasticBeanstalk.DeleteApplicationVersion
import Network.AWS.ElasticBeanstalk.UpdateApplicationVersion
import Network.AWS.ElasticBeanstalk.CreateConfigurationTemplate
import Network.AWS.ElasticBeanstalk.RebuildEnvironment
import Network.AWS.ElasticBeanstalk.DeleteEnvironmentConfiguration
import Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs
import Network.AWS.ElasticBeanstalk.ListAvailableSolutionStacks
import Network.AWS.ElasticBeanstalk.DescribeConfigurationOptions
import Network.AWS.ElasticBeanstalk.CreateStorageLocation
import Network.AWS.ElasticBeanstalk.DescribeConfigurationSettings
import Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings
import Network.AWS.ElasticBeanstalk.RestartAppServer
import Network.AWS.ElasticBeanstalk.DescribeEnvironments
import Network.AWS.ElasticBeanstalk.CheckDNSAvailability
import Network.AWS.ElasticBeanstalk.DescribeApplicationVersions
import Network.AWS.ElasticBeanstalk.CreateEnvironment
