{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns AWS resources for this environment.
-- https://elasticbeanstalk.us-east-1.amazon.com/?EnvironmentId=e-hc8mvnayrx
-- &EnvironmentName=SampleAppVersion &Operation=DescribeEnvironmentResources
-- &AuthParams elasticbeanstalk-SampleAppVersion
-- elasticbeanstalk-SampleAppVersion-hbAc8cSZH7
-- elasticbeanstalk-SampleAppVersion-us-east-1c SampleAppVersion
-- elasticbeanstalk-SampleAppVersion-us-east-1c
-- e1cb7b96-f287-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources where

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
describeEnvironmentResources :: DescribeEnvironmentResources
describeEnvironmentResources = DescribeEnvironmentResources
    { dermEnvironmentId = Nothing
    , dermEnvironmentName = Nothing
    }

data DescribeEnvironmentResources = DescribeEnvironmentResources
    { dermEnvironmentId :: Maybe Text
      -- ^ The ID of the environment to retrieve AWS resource usage data. Condition:
      -- You must specify either this or an EnvironmentName, or both. If you do not
      -- specify either, AWS Elastic Beanstalk returns MissingRequiredParameter
      -- error.
    , dermEnvironmentName :: Maybe Text
      -- ^ The name of the environment to retrieve AWS resource usage data. Condition:
      -- You must specify either this or an EnvironmentId, or both. If you do not
      -- specify either, AWS Elastic Beanstalk returns MissingRequiredParameter
      -- error.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeEnvironmentResources

instance AWSRequest DescribeEnvironmentResources where
    type Er DescribeEnvironmentResources = ElasticBeanstalkError
    type Rs DescribeEnvironmentResources = DescribeEnvironmentResourcesResponse
    request = getQuery service "DescribeEnvironmentResources"

data DescribeEnvironmentResourcesResponse = DescribeEnvironmentResourcesResponse
    { dermrsEnvironmentResources :: Maybe EnvironmentResourceDescription
      -- ^ A list of EnvironmentResourceDescription.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeEnvironmentResourcesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeEnvironmentResourcesResponse"
        :| ["DescribeEnvironmentResourcesResult"]
