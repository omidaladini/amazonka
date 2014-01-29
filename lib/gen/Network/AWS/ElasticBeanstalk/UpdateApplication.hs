{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.UpdateApplication
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the specified application to have the specified properties. If a
-- property (for example, description) is not provided, the value remains
-- unchanged. To clear these properties, specify an empty string.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &Description=Another%20Description &Operation=UpdateApplication &AuthParams
-- New Version Another Description SampleApp 2010-11-17T19:26:20.410Z
-- 2010-11-17T20:42:54.611Z Default 40be666b-f28b-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.UpdateApplication where

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
updateApplication :: Text
                  -> AWS (Either ElasticBeanstalkError UpdateApplicationResponse)
updateApplication p1 = undefined $ UpdateApplication
    { uamApplicationName = p1
    , uamDescription = Nothing
    }

data UpdateApplication = UpdateApplication
    { uamApplicationName :: !Text
      -- ^ The name of the application to update. If no such application is found,
      -- UpdateApplication returns an InvalidParameterValue error.
    , uamDescription :: Maybe Text
      -- ^ A new description for the application. Default: If not specified, AWS
      -- Elastic Beanstalk does not update the description.
    } deriving (Eq, Show, Generic)

instance ToQuery UpdateApplication

instance AWSRequest UpdateApplication where
    type Er UpdateApplication = ElasticBeanstalkError
    type Rs UpdateApplication = UpdateApplicationResponse
    request = getQuery service "UpdateApplication"

data UpdateApplicationResponse = UpdateApplicationResponse
    { uamrsApplication :: Maybe ApplicationDescription
      -- ^ The ApplicationDescription of the application.
    } deriving (Eq, Show, Generic)

instance FromXML UpdateApplicationResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "UpdateApplicationResponse"
        :| ["UpdateApplicationResult"]
