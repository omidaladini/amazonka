{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.UpdateApplicationVersion
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the specified application version to have the specified properties.
-- If a property (for example, description) is not provided, the value remains
-- unchanged. To clear properties, specify an empty string.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &VersionLabel=New%20Version &Description=New%20Release%20Description
-- &Operation=UpdateApplicationVersion &AuthParams awsemr sample.war New
-- Version New Release Description SampleApp 2010-11-17T19:26:20.699Z
-- 2010-11-17T20:48:16.632Z 00b10aa1-f28c-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.UpdateApplicationVersion where

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
updateApplicationVersion :: Text
                         -> Text
                         -> AWS (Either ElasticBeanstalkError UpdateApplicationVersionResponse)
updateApplicationVersion p1 p2 = undefined $ UpdateApplicationVersion
    { uavmApplicationName = p1
    , uavmVersionLabel = p2
    , uavmDescription = Nothing
    }

data UpdateApplicationVersion = UpdateApplicationVersion
    { uavmApplicationName :: !Text
      -- ^ The name of the application associated with this version. If no application
      -- is found with this name, UpdateApplication returns an InvalidParameterValue
      -- error.
    , uavmDescription :: Maybe Text
      -- ^ A new description for this release.
    , uavmVersionLabel :: !Text
      -- ^ The name of the version to update. If no application version is found with
      -- this label, UpdateApplication returns an InvalidParameterValue error.
    } deriving (Eq, Show, Generic)

instance ToQuery UpdateApplicationVersion

instance AWSRequest UpdateApplicationVersion where
    type Er UpdateApplicationVersion = ElasticBeanstalkError
    type Rs UpdateApplicationVersion = UpdateApplicationVersionResponse
    request = getQuery service "UpdateApplicationVersion"

data UpdateApplicationVersionResponse = UpdateApplicationVersionResponse
    { uavmrsApplicationVersion :: Maybe ApplicationVersionDescription
      -- ^ The ApplicationVersionDescription of the application version.
    } deriving (Eq, Show, Generic)

instance FromXML UpdateApplicationVersionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "UpdateApplicationVersionResponse"
        :| ["UpdateApplicationVersionResult"]
