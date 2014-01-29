{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.DescribeApplicationVersions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns descriptions for existing application versions.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &Operation=DescribeApplicationVersions &AuthParams amazonaws.com sample.war
-- Version1 description SampleApp 2010-11-17T03:21:59.161Z
-- 2010-11-17T03:21:59.161Z 773cd80a-f26c-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.DescribeApplicationVersions where

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
describeApplicationVersions :: AWS (Either ElasticBeanstalkError DescribeApplicationVersionsResponse)
describeApplicationVersions = undefined $ DescribeApplicationVersions
    { davnApplicationName = Nothing
    , davnVersionLabels = []
    }

data DescribeApplicationVersions = DescribeApplicationVersions
    { davnApplicationName :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned descriptions to
      -- only include ones that are associated with the specified application.
    , davnVersionLabels :: [Text]
      -- ^ If specified, restricts the returned descriptions to only include ones that
      -- have the specified version labels.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeApplicationVersions

instance AWSRequest DescribeApplicationVersions where
    type Er DescribeApplicationVersions = ElasticBeanstalkError
    type Rs DescribeApplicationVersions = DescribeApplicationVersionsResponse
    request = getQuery service "DescribeApplicationVersions"

data DescribeApplicationVersionsResponse = DescribeApplicationVersionsResponse
    { davnrsApplicationVersions :: [ApplicationVersionDescription]
      -- ^ A list of ApplicationVersionDescription .
    } deriving (Eq, Show, Generic)

instance FromXML DescribeApplicationVersionsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeApplicationVersionsResponse"
        :| ["DescribeApplicationVersionsResult"]
