{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.RestartAppServer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Causes the environment to restart the application container server running
-- on each Amazon EC2 instance.
-- https://elasticbeanstalk.us-east-1.amazon.com/?EnvironmentId=e-hc8mvnayrx
-- &EnvironmentName=SampleAppVersion &Operation=RestartAppServer &AuthParams
-- 90e8d1d5-f28a-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.RestartAppServer where

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
restartAppServer :: AWS (Either ElasticBeanstalkError RestartAppServerResponse)
restartAppServer = undefined $ RestartAppServer
    { rasmEnvironmentId = Nothing
    , rasmEnvironmentName = Nothing
    }

data RestartAppServer = RestartAppServer
    { rasmEnvironmentId :: Maybe Text
      -- ^ The ID of the environment to restart the server for. Condition: You must
      -- specify either this or an EnvironmentName, or both. If you do not specify
      -- either, AWS Elastic Beanstalk returns MissingRequiredParameter error.
    , rasmEnvironmentName :: Maybe Text
      -- ^ The name of the environment to restart the server for. Condition: You must
      -- specify either this or an EnvironmentId, or both. If you do not specify
      -- either, AWS Elastic Beanstalk returns MissingRequiredParameter error.
    } deriving (Eq, Show, Generic)

instance ToQuery RestartAppServer

instance AWSRequest RestartAppServer where
    type Er RestartAppServer = ElasticBeanstalkError
    type Rs RestartAppServer = RestartAppServerResponse
    request = getQuery service "RestartAppServer"

data RestartAppServerResponse = RestartAppServerResponse
    deriving (Eq, Show, Generic)

instance FromXML RestartAppServerResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "RestartAppServerResponse"
        :| ["RestartAppServerResult"]
