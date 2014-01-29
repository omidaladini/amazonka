{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.CreateApplication
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an application that has one configuration template named default
-- and no application versions. The default configuration template is for a
-- 32-bit version of the Amazon Linux operating system running the Tomcat 6
-- application container.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &Description=Sample%20Description &Operation=CreateApplication &AuthParams
-- Sample Description SampleApp 2010-11-16T23:09:20.256Z
-- 2010-11-16T23:09:20.256Z Default 8b00e053-f1d6-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.CreateApplication where

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
createApplication :: Text
                  -> AWS (Either ElasticBeanstalkError CreateApplicationResponse)
createApplication p1 = undefined $ CreateApplication
    { camApplicationName = p1
    , camDescription = Nothing
    }

data CreateApplication = CreateApplication
    { camApplicationName :: !Text
      -- ^ The name of the application. Constraint: This name must be unique within
      -- your account. If the specified name already exists, the action returns an
      -- InvalidParameterValue error.
    , camDescription :: Maybe Text
      -- ^ Describes the application.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateApplication

instance AWSRequest CreateApplication where
    type Er CreateApplication = ElasticBeanstalkError
    type Rs CreateApplication = CreateApplicationResponse
    request = getQuery service "CreateApplication"

data CreateApplicationResponse = CreateApplicationResponse
    { camrsApplication :: Maybe ApplicationDescription
      -- ^ The ApplicationDescription of the application.
    } deriving (Eq, Show, Generic)

instance FromXML CreateApplicationResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateApplicationResponse"
        :| ["CreateApplicationResult"]
