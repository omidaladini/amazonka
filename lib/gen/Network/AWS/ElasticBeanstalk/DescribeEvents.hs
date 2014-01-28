{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.DescribeEvents
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns list of event descriptions matching criteria up to the last 6
-- weeks. This action returns the most recent 1,000 events from the specified
-- NextToken.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &Severity=TRACE &StartTime=2010-11-17T10%3A26%3A40Z
-- &Operation=DescribeEvents &AuthParams Successfully completed
-- createEnvironment activity. 2010-11-17T20:25:35.191Z New Version
-- bb01fa74-f287-11df-8a78-9f77047e0d0c SampleApp SampleAppVersion INFO
-- Launching a new EC2 instance: i-04a8c569 2010-11-17T20:21:30Z New Version
-- SampleApp SampleAppVersion DEBUG At least one EC2 instance has entered the
-- InService lifecycle state. 2010-11-17T20:20:32.008Z New Version
-- bb01fa74-f287-11df-8a78-9f77047e0d0c SampleApp SampleAppVersion INFO
-- Elastic Load Balancer elasticbeanstalk-SampleAppVersion has failed 0
-- healthy instances - Environment may not be available. 2010-11-17T20:19:28Z
-- New Version SampleApp SampleAppVersion WARN
-- f10d02dd-f288-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.DescribeEvents where

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

data DescribeEvents = DescribeEvents
    { demApplicationName :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned descriptions to
      -- include only those associated with this application.
    , demEndTime :: Maybe UTCTime
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned descriptions to
      -- those that occur up to, but not including, the EndTime.
    , demEnvironmentId :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned descriptions to
      -- those associated with this environment.
    , demEnvironmentName :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned descriptions to
      -- those associated with this environment.
    , demMaxRecords :: Maybe Int
      -- ^ Specifies the maximum number of events that can be returned, beginning with
      -- the most recent event.
    , demNextToken :: Maybe Text
      -- ^ Pagination token. If specified, the events return the next batch of
      -- results.
    , demRequestId :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the described events to
      -- include only those associated with this request ID.
    , demSeverity :: Maybe EventSeverity
      -- ^ If specified, limits the events returned from this call to include only
      -- those with the specified severity or higher.
    , demStartTime :: Maybe UTCTime
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned descriptions to
      -- those that occur on or after this time.
    , demTemplateName :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned descriptions to
      -- those that are associated with this environment configuration.
    , demVersionLabel :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned descriptions to
      -- those associated with this application version.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeEvents

instance AWSRequest DescribeEvents where
    type Er DescribeEvents = ElasticBeanstalkError
    type Rs DescribeEvents = DescribeEventsResponse
    request = getQuery service "DescribeEvents"

instance AWSPager DescribeEvents where
    next rq rs
        | Just x <- demrsNextToken rs = Just $ rq { demNextToken = Just x }
        | otherwise = Nothing

data DescribeEventsResponse = DescribeEventsResponse
    { demrsEvents :: [EventDescription]
      -- ^ A list of EventDescription.
    , demrsNextToken :: Maybe Text
      -- ^ If returned, this indicates that there are more results to obtain. Use this
      -- token in the next DescribeEvents call to get the next batch of events.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeEventsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeEventsResponse"
        :| ["DescribeEventsResult"]
