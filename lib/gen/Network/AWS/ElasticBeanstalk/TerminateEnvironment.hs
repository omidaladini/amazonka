{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.TerminateEnvironment
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Terminates the specified environment.
-- https://elasticbeanstalk.us-east-1.amazon.com/?EnvironmentId=e-icsgecu3wf
-- &EnvironmentName=SampleApp &TerminateResources=true
-- &Operation=TerminateEnvironment &AuthParams Version1 Terminating SampleApp
-- elasticbeanstalk-SampleApp-1394386994.us-east-1.elb.amazonaws.com
-- SampleApp-jxb293wg7n.elasticbeanstalk.amazonaws.com Grey e-icsgecu3wf
-- 2010-11-17T17:10:41.976Z 32bit Amazon Linux running Tomcat 7 EnvDescrip
-- SampleApp 2010-11-17T03:59:33.520Z 9b71af21-f26d-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.TerminateEnvironment where

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

data TerminateEnvironment = TerminateEnvironment
    { temEnvironmentId :: Maybe Text
      -- ^ The ID of the environment to terminate. Condition: You must specify either
      -- this or an EnvironmentName, or both. If you do not specify either, AWS
      -- Elastic Beanstalk returns MissingRequiredParameter error.
    , temEnvironmentName :: Maybe Text
      -- ^ The name of the environment to terminate. Condition: You must specify
      -- either this or an EnvironmentId, or both. If you do not specify either, AWS
      -- Elastic Beanstalk returns MissingRequiredParameter error.
    , temTerminateResources :: Maybe Bool
      -- ^ Indicates whether the associated AWS resources should shut down when the
      -- environment is terminated: true: (default) The user AWS resources (for
      -- example, the Auto Scaling group, LoadBalancer, etc.) are terminated along
      -- with the environment. false: The environment is removed from the AWS
      -- Elastic Beanstalk but the AWS resources continue to operate. true: The
      -- specified environment as well as the associated AWS resources, such as Auto
      -- Scaling group and LoadBalancer, are terminated. false: AWS Elastic
      -- Beanstalk resource management is removed from the environment, but the AWS
      -- resources continue to operate. For more information, see the AWS Elastic
      -- Beanstalk User Guide. Default: true Valid Values: true | false.
    } deriving (Eq, Show, Generic)

instance ToQuery TerminateEnvironment

instance AWSRequest TerminateEnvironment where
    type Er TerminateEnvironment = ElasticBeanstalkError
    type Rs TerminateEnvironment = TerminateEnvironmentResponse
    request = getQuery service "TerminateEnvironment"

data TerminateEnvironmentResponse = TerminateEnvironmentResponse
    { temrsApplicationName :: Maybe Text
      -- ^ The name of the application associated with this environment.
    , temrsCNAME :: Maybe Text
      -- ^ The URL to the CNAME for this environment.
    , temrsDateCreated :: Maybe UTCTime
      -- ^ The creation date for this environment.
    , temrsDateUpdated :: Maybe UTCTime
      -- ^ The last modified date for this environment.
    , temrsDescription :: Maybe Text
      -- ^ Describes this environment.
    , temrsEndpointURL :: Maybe Text
      -- ^ The URL to the LoadBalancer for this environment.
    , temrsEnvironmentId :: Maybe Text
      -- ^ The ID of this environment.
    , temrsEnvironmentName :: Maybe Text
      -- ^ The name of this environment.
    , temrsHealth :: Maybe EnvironmentHealth
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
    , temrsResources :: Maybe EnvironmentResourcesDescription
      -- ^ The description of the AWS resources used by this environment.
    , temrsSolutionStackName :: Maybe Text
      -- ^ The name of the SolutionStack deployed with this environment.
    , temrsStatus :: Maybe EnvironmentStatus
      -- ^ The current operational status of the environment: Launching: Environment
      -- is in the process of initial deployment. Updating: Environment is in the
      -- process of updating its configuration settings or application version.
      -- Ready: Environment is available to have an action performed on it, such as
      -- update or terminate. Terminating: Environment is in the shut-down process.
      -- Terminated: Environment is not running.
    , temrsTemplateName :: Maybe Text
      -- ^ The name of the configuration template used to originally launch this
      -- environment.
    , temrsTier :: Maybe EnvironmentTier
    , temrsVersionLabel :: Maybe Text
      -- ^ The application version deployed in this environment.
    } deriving (Eq, Show, Generic)

instance FromXML TerminateEnvironmentResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "TerminateEnvironmentResponse"
        :| ["TerminateEnvironmentResult"]
