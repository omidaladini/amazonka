{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.DeleteEnvironmentConfiguration
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the draft configuration associated with the running environment.
-- Updating a running environment with any configuration changes creates a
-- draft configuration set. You can get the draft configuration using
-- DescribeConfigurationSettings while the update is in progress or if the
-- update fails. The DeploymentStatus for the draft configuration indicates
-- whether the deployment is in process or has failed. The draft configuration
-- remains in existence until it is deleted with this action.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &EnvironmentName=SampleApp &Operation=DeleteEnvironmentConfiguration
-- &AuthParams fdf76507-f26d-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.DeleteEnvironmentConfiguration where

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

-- | Convenience method utilising default fields where applicable.
deleteEnvironmentConfiguration :: Text
                               -> Text
                               -> AWS (Either ElasticBeanstalkError DeleteEnvironmentConfigurationResponse)
deleteEnvironmentConfiguration p1 p2 = undefined $ DeleteEnvironmentConfiguration
    { decmApplicationName = p1
    , decmEnvironmentName = p2
    }

data DeleteEnvironmentConfiguration = DeleteEnvironmentConfiguration
    { decmApplicationName :: !Text
      -- ^ The name of the application the environment is associated with.
    , decmEnvironmentName :: !Text
      -- ^ The name of the environment to delete the draft configuration from.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteEnvironmentConfiguration

instance AWSRequest DeleteEnvironmentConfiguration where
    type Er DeleteEnvironmentConfiguration = ElasticBeanstalkError
    type Rs DeleteEnvironmentConfiguration = DeleteEnvironmentConfigurationResponse
    request = getQuery service "DeleteEnvironmentConfiguration"

data DeleteEnvironmentConfigurationResponse = DeleteEnvironmentConfigurationResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteEnvironmentConfigurationResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeleteEnvironmentConfigurationResponse"
        :| ["DeleteEnvironmentConfigurationResult"]
