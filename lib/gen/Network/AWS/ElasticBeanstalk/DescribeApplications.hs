{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.DescribeApplications
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the descriptions of existing applications.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationNames.member.1=SampleApplication
-- &Operation=DescribeApplications &AuthParams Sample Description
-- SampleApplication 2010-11-16T20:20:51.974Z 2010-11-16T20:20:51.974Z Default
-- 577c70ff-f1d7-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.DescribeApplications where

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

data DescribeApplications = DescribeApplications
    { damApplicationNames :: [Text]
      -- ^ If specified, AWS Elastic Beanstalk restricts the returned descriptions to
      -- only include those with the specified names.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeApplications

instance AWSRequest DescribeApplications where
    type Er DescribeApplications = ElasticBeanstalkError
    type Rs DescribeApplications = DescribeApplicationsResponse
    request = getQuery service "DescribeApplications"

data DescribeApplicationsResponse = DescribeApplicationsResponse
    { damrsApplications :: [ApplicationDescription]
      -- ^ This parameter contains a list of ApplicationDescription.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeApplicationsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeApplicationsResponse"
        :| ["DescribeApplicationsResult"]
