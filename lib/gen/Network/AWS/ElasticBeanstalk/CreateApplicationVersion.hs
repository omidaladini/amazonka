{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.CreateApplicationVersion
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an application version for the specified application. Once you
-- create an application version with a specified Amazon S3 bucket and key
-- location, you cannot change that Amazon S3 location. If you change the
-- Amazon S3 location, you receive an exception when you attempt to launch an
-- environment from the application version.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &VersionLabel=Version1 &Description=description
-- &SourceBundle.S3Bucket=amazonaws.com &SourceBundle.S3Key=sample.war
-- &AutoCreateApplication=true &Operation=CreateApplicationVersion &AuthParams
-- amazonaws.com sample.war Version1 description SampleApp
-- 2010-11-17T03:21:59.161Z 2010-11-17T03:21:59.161Z
-- d653efef-f1f9-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.CreateApplicationVersion where

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
createApplicationVersion :: Text
                         -> Text
                         -> CreateApplicationVersion
createApplicationVersion p1 p2 = undefined $ CreateApplicationVersion
    { cavmApplicationName = p1
    , cavmVersionLabel = p2
    , cavmAutoCreateApplication = Nothing
    , cavmDescription = Nothing
    , cavmSourceBundle = Nothing
    }

data CreateApplicationVersion = CreateApplicationVersion
    { cavmApplicationName :: !Text
      -- ^ The name of the application. If no application is found with this name, and
      -- AutoCreateApplication is false, returns an InvalidParameterValue error.
    , cavmAutoCreateApplication :: Maybe Bool
      -- ^ Determines how the system behaves if the specified application for this
      -- version does not already exist: true: Automatically creates the specified
      -- application for this version if it does not already exist. false: Returns
      -- an InvalidParameterValue if the specified application for this version does
      -- not already exist. true : Automatically creates the specified application
      -- for this release if it does not already exist. false : Throws an
      -- InvalidParameterValue if the specified application for this release does
      -- not already exist. Default: false Valid Values: true | false.
    , cavmDescription :: Maybe Text
      -- ^ Describes this version.
    , cavmSourceBundle :: Maybe S3Location
      -- ^ The Amazon S3 bucket and key that identify the location of the source
      -- bundle for this version. If data found at the Amazon S3 location exceeds
      -- the maximum allowed source bundle size, AWS Elastic Beanstalk returns an
      -- InvalidParameterValue error. The maximum size allowed is 512 MB. Default:
      -- If not specified, AWS Elastic Beanstalk uses a sample application. If only
      -- partially specified (for example, a bucket is provided but not the key) or
      -- if no data is found at the Amazon S3 location, AWS Elastic Beanstalk
      -- returns an InvalidParameterCombination error.
    , cavmVersionLabel :: !Text
      -- ^ A label identifying this version. Constraint: Must be unique per
      -- application. If an application version already exists with this label for
      -- the specified application, AWS Elastic Beanstalk returns an
      -- InvalidParameterValue error.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateApplicationVersion

instance AWSRequest CreateApplicationVersion where
    type Er CreateApplicationVersion = ElasticBeanstalkError
    type Rs CreateApplicationVersion = CreateApplicationVersionResponse
    request = getQuery service "CreateApplicationVersion"

data CreateApplicationVersionResponse = CreateApplicationVersionResponse
    { cavmrsApplicationVersion :: Maybe ApplicationVersionDescription
      -- ^ The ApplicationVersionDescription of the application version.
    } deriving (Eq, Show, Generic)

instance FromXML CreateApplicationVersionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateApplicationVersionResponse"
        :| ["CreateApplicationVersionResult"]
