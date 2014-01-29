{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.RebuildEnvironment
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes and recreates all of the AWS resources (for example: the Auto
-- Scaling group, load balancer, etc.) for a specified environment and forces
-- a restart.
-- https://elasticbeanstalk.us-east-1.amazon.com/?EnvironmentId=e-hc8mvnayrx
-- &EnvironmentName=SampleAppVersion &Operation=RebuildEnvironment &AuthParams
-- a7d6606e-f289-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.RebuildEnvironment where

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
rebuildEnvironment :: AWS (Either ElasticBeanstalkError RebuildEnvironmentResponse)
rebuildEnvironment = undefined $ RebuildEnvironment
    { remEnvironmentId = Nothing
    , remEnvironmentName = Nothing
    }

data RebuildEnvironment = RebuildEnvironment
    { remEnvironmentId :: Maybe Text
      -- ^ The ID of the environment to rebuild. Condition: You must specify either
      -- this or an EnvironmentName, or both. If you do not specify either, AWS
      -- Elastic Beanstalk returns MissingRequiredParameter error.
    , remEnvironmentName :: Maybe Text
      -- ^ The name of the environment to rebuild. Condition: You must specify either
      -- this or an EnvironmentId, or both. If you do not specify either, AWS
      -- Elastic Beanstalk returns MissingRequiredParameter error.
    } deriving (Eq, Show, Generic)

instance ToQuery RebuildEnvironment

instance AWSRequest RebuildEnvironment where
    type Er RebuildEnvironment = ElasticBeanstalkError
    type Rs RebuildEnvironment = RebuildEnvironmentResponse
    request = getQuery service "RebuildEnvironment"

data RebuildEnvironmentResponse = RebuildEnvironmentResponse
    deriving (Eq, Show, Generic)

instance FromXML RebuildEnvironmentResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "RebuildEnvironmentResponse"
        :| ["RebuildEnvironmentResult"]
