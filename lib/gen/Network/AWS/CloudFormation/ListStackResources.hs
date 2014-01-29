{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.ListStackResources
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns descriptions of all resources of the specified stack. For deleted
-- stacks, ListStackResources returns resource information for up to 90 days
-- after the stack has been deleted.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=ListStackResources
-- &StackName=MyStack &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2011-07-08T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature] CREATE_COMPLETE DBSecurityGroup 2011-06-21T20:15:58Z
-- gmarcteststack-dbsecuritygroup-1s5m0ez5lkk6w AWS::RDS::DBSecurityGroup
-- CREATE_COMPLETE SampleDB 2011-06-21T20:25:57Z MyStack-sampledb-ycwhk1v830lx
-- AWS::RDS::DBInstance CREATE_COMPLETE SampleApplication 2011-06-21T20:26:12Z
-- MyStack-SampleApplication-1MKNASYR3RBQL AWS::ElasticBeanstalk::Application
-- CREATE_COMPLETE SampleEnvironment 2011-06-21T20:28:48Z
-- myst-Samp-1AGU6ERZX6M3Q AWS::ElasticBeanstalk::Environment CREATE_COMPLETE
-- AlarmTopic 2011-06-21T20:29:06Z
-- arn:aws:sns:us-east-1:803981987763:MyStack-AlarmTopic-SW4IQELG7RPJ
-- AWS::SNS::Topic CREATE_COMPLETE CPUAlarmHigh 2011-06-21T20:29:23Z
-- MyStack-CPUAlarmHigh-POBWQPDJA81F AWS::CloudWatch::Alarm
-- 2d06e36c-ac1d-11e0-a958-f9382b6eb86b.
module Network.AWS.CloudFormation.ListStackResources where

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

import Network.AWS.CloudFormation.Service
import Network.AWS.CloudFormation.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
listStackResources :: Text
                   -> ListStackResources
listStackResources p1 = undefined $ ListStackResources
    { lsriStackName = p1
    , lsriNextToken = Nothing
    }

data ListStackResources = ListStackResources
    { lsriNextToken :: Maybe Text
      -- ^ String that identifies the start of the next list of stack resource
      -- summaries, if there is one. Default: There is no default value.
    , lsriStackName :: !Text
      -- ^ The name or the unique identifier associated with the stack, which are not
      -- always interchangeable: Running stacks: You can specify either the stack's
      -- name or its unique stack ID. Deleted stacks: You must specify the unique
      -- stack ID. Default: There is no default value.
    } deriving (Eq, Show, Generic)

instance ToQuery ListStackResources

instance AWSRequest ListStackResources where
    type Er ListStackResources = CloudFormationError
    type Rs ListStackResources = ListStackResourcesResponse
    request = getQuery service "ListStackResources"

instance AWSPager ListStackResources where
    next rq rs
        | Just x <- lsrirsNextToken rs = Just $ rq { lsriNextToken = Just x }
        | otherwise = Nothing

data ListStackResourcesResponse = ListStackResourcesResponse
    { lsrirsNextToken :: Maybe Text
      -- ^ String that identifies the start of the next list of events, if there is
      -- one.
    , lsrirsStackResourceSummaries :: [StackResourceSummary]
      -- ^ A list of StackResourceSummary structures.
    } deriving (Eq, Show, Generic)

instance FromXML ListStackResourcesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ListStackResourcesResponse"
        :| ["ListStackResourcesResult"]
