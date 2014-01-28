{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironments
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns descriptions for existing environments.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &IncludeDeleted=true &IncludedDeletedBackTo=2008-11-05T06%3A00%3A00Z
-- &Operation=DescribeEnvironments &AuthParams Version1 Available SampleApp
-- elasticbeanstalk-SampleApp-1394386994.us-east-1.elb.amazonaws.com
-- SampleApp-jxb293wg7n.elasticbeanstalk.amazonaws.com Green e-icsgecu3wf
-- 2010-11-17T04:01:40.668Z 32bit Amazon Linux running Tomcat 7 EnvDescrip
-- SampleApp 2010-11-17T03:59:33.520Z 44790c68-f260-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.DescribeEnvironments where

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

data DescribeEnvironments = DescribeEnvironments
    { denApplicationName :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned descriptions to
      -- include only those that are associated with this application.
    , denEnvironmentIds :: [Text]
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned descriptions to
      -- include only those that have the specified IDs.
    , denEnvironmentNames :: [Text]
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned descriptions to
      -- include only those that have the specified names.
    , denIncludeDeleted :: Maybe Bool
      -- ^ Indicates whether to include deleted environments: true: Environments that
      -- have been deleted after IncludedDeletedBackTo are displayed. false: Do not
      -- include deleted environments.
    , denIncludedDeletedBackTo :: Maybe UTCTime
      -- ^ If specified when IncludeDeleted is set to true, then environments deleted
      -- after this date are displayed.
    , denVersionLabel :: Maybe Text
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned descriptions to
      -- include only those that are associated with this application version.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeEnvironments

instance AWSRequest DescribeEnvironments where
    type Er DescribeEnvironments = ElasticBeanstalkError
    type Rs DescribeEnvironments = DescribeEnvironmentsResponse
    request = getQuery service "DescribeEnvironments"

data DescribeEnvironmentsResponse = DescribeEnvironmentsResponse
    { denrsEnvironments :: [EnvironmentDescription]
      -- ^ Returns an EnvironmentDescription list.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeEnvironmentsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeEnvironmentsResponse"
        :| ["DescribeEnvironmentsResult"]
