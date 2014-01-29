{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.CreateEnvironment
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Launches an environment for the specified application using the specified
-- configuration.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &EnvironmentName=SampleApp
-- &SolutionStackName=32bit%20Amazon%20Linux%20running%20Tomcat%207
-- &Description=EnvDescrip &Operation=CreateEnvironment &AuthParams Version1
-- Deploying SampleApp Grey e-icsgecu3wf 2010-11-17T03:59:33.520Z 32bit Amazon
-- Linux running Tomcat 7 EnvDescrip SampleApp 2010-11-17T03:59:33.520Z
-- 15db925e-f1ff-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.CreateEnvironment where

import           Control.Monad
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import           Text.XML.Generic

import Network.AWS.ElasticBeanstalk.Service
import Network.AWS.ElasticBeanstalk.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createEnvironment :: Text
                  -> Text
                  -> CreateEnvironment
createEnvironment p1 p2 = undefined $ CreateEnvironment
    { cemApplicationName = p1
    , cemEnvironmentName = p2
    , cemCNAMEPrefix = Nothing
    , cemDescription = Nothing
    , cemOptionSettings = []
    , cemOptionsToRemove = []
    , cemSolutionStackName = Nothing
    , cemTemplateName = Nothing
    , cemTier = Nothing
    , cemVersionLabel = Nothing
    }

data CreateEnvironment = CreateEnvironment
    { cemApplicationName :: !Text
      -- ^ The name of the application that contains the version to be deployed. If no
      -- application is found with this name, CreateEnvironment returns an
      -- InvalidParameterValue error.
    , cemCNAMEPrefix :: Maybe Text
      -- ^ If specified, the environment attempts to use this value as the prefix for
      -- the CNAME. If not specified, the CNAME is generated automatically by
      -- appending a random alphanumeric string to the environment name.
    , cemDescription :: Maybe Text
      -- ^ Describes this environment.
    , cemEnvironmentName :: !Text
      -- ^ A unique name for the deployment environment. Used in the application URL.
      -- Constraint: Must be from 4 to 23 characters in length. The name can contain
      -- only letters, numbers, and hyphens. It cannot start or end with a hyphen.
      -- This name must be unique in your account. If the specified name already
      -- exists, AWS Elastic Beanstalk returns an InvalidParameterValue error.
      -- Default: If the CNAME parameter is not specified, the environment name
      -- becomes part of the CNAME, and therefore part of the visible URL for your
      -- application.
    , cemOptionSettings :: [ConfigurationOptionSetting]
      -- ^ If specified, AWS Elastic Beanstalk sets the specified configuration
      -- options to the requested value in the configuration set for the new
      -- environment. These override the values obtained from the solution stack or
      -- the configuration template.
    , cemOptionsToRemove :: [OptionSpecification]
      -- ^ A list of custom user-defined configuration options to remove from the
      -- configuration set for this new environment.
    , cemSolutionStackName :: Maybe Text
      -- ^ This is an alternative to specifying a configuration name. If specified,
      -- AWS Elastic Beanstalk sets the configuration values to the default values
      -- associated with the specified solution stack. Condition: You must specify
      -- either this or a TemplateName, but not both. If you specify both, AWS
      -- Elastic Beanstalk returns an InvalidParameterCombination error. If you do
      -- not specify either, AWS Elastic Beanstalk returns a
      -- MissingRequiredParameter error.
    , cemTemplateName :: Maybe Text
      -- ^ The name of the configuration template to use in deployment. If no
      -- configuration template is found with this name, AWS Elastic Beanstalk
      -- returns an InvalidParameterValue error. Condition: You must specify either
      -- this parameter or a SolutionStackName, but not both. If you specify both,
      -- AWS Elastic Beanstalk returns an InvalidParameterCombination error. If you
      -- do not specify either, AWS Elastic Beanstalk returns a
      -- MissingRequiredParameter error.
    , cemTier :: Maybe EnvironmentTier
    , cemVersionLabel :: Maybe Text
      -- ^ The name of the application version to deploy. If the specified application
      -- has no associated application versions, AWS Elastic Beanstalk
      -- UpdateEnvironment returns an InvalidParameterValue error. Default: If not
      -- specified, AWS Elastic Beanstalk attempts to launch the most recently
      -- created application version.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateEnvironment

instance AWSRequest CreateEnvironment where
    type Er CreateEnvironment = ElasticBeanstalkError
    type Rs CreateEnvironment = CreateEnvironmentResponse
    request = getQuery service "CreateEnvironment"

data CreateEnvironmentResponse = CreateEnvironmentResponse
    { cemrsApplicationName :: Maybe Text
      -- ^ The name of the application associated with this environment.
    , cemrsCNAME :: Maybe Text
      -- ^ The URL to the CNAME for this environment.
    , cemrsDateCreated :: Maybe UTCTime
      -- ^ The creation date for this environment.
    , cemrsDateUpdated :: Maybe UTCTime
      -- ^ The last modified date for this environment.
    , cemrsDescription :: Maybe Text
      -- ^ Describes this environment.
    , cemrsEndpointURL :: Maybe Text
      -- ^ The URL to the LoadBalancer for this environment.
    , cemrsEnvironmentId :: Maybe Text
      -- ^ The ID of this environment.
    , cemrsEnvironmentName :: Maybe Text
      -- ^ The name of this environment.
    , cemrsHealth :: Maybe EnvironmentHealth
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
    , cemrsResources :: Maybe EnvironmentResourcesDescription
      -- ^ The description of the AWS resources used by this environment.
    , cemrsSolutionStackName :: Maybe Text
      -- ^ The name of the SolutionStack deployed with this environment.
    , cemrsStatus :: Maybe EnvironmentStatus
      -- ^ The current operational status of the environment: Launching: Environment
      -- is in the process of initial deployment. Updating: Environment is in the
      -- process of updating its configuration settings or application version.
      -- Ready: Environment is available to have an action performed on it, such as
      -- update or terminate. Terminating: Environment is in the shut-down process.
      -- Terminated: Environment is not running.
    , cemrsTemplateName :: Maybe Text
      -- ^ The name of the configuration template used to originally launch this
      -- environment.
    , cemrsTier :: Maybe EnvironmentTier
    , cemrsVersionLabel :: Maybe Text
      -- ^ The application version deployed in this environment.
    } deriving (Eq, Show, Generic)

instance FromXML CreateEnvironmentResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateEnvironmentResponse"
        :| ["CreateEnvironmentResult"]
