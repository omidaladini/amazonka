{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.DescribeStackEvents
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns all stack related events for a specified stack. For more
-- information about a stack's event history, go to the AWS CloudFormation
-- User Guide. Events are returned, even if the stack never existed or has
-- been successfully deleted. https://cloudformation.us-east-1.amazonaws.com/
-- ?Action=DescribeStackEvents &StackName=MyStack &Version=2010-05-15
-- &SignatureVersion=2 &Timestamp=2010-07-27T22%3A26%3A28.000Z
-- &AWSAccessKeyId=[AWS Access KeyID] &Signature=[Signature] Event-1-Id
-- arn:aws:cloudformation:us-east-1:123456789:stack/MyStack/aaf549a0-a413-11df-adb3-5081b3858e83
-- MyStack MyStack MyStack_One AWS::CloudFormation::Stack 2010-07-27T22:26:28Z
-- CREATE_IN_PROGRESS User initiated Event-2-Id
-- arn:aws:cloudformation:us-east-1:123456789:stack/MyStack/aaf549a0-a413-11df-adb3-5081b3858e83
-- MyStack MyDBInstance MyStack_DB1 AWS::SecurityGroup 2010-07-27T22:27:28Z
-- CREATE_IN_PROGRESS {"GroupDescription":...} Event-3-Id
-- arn:aws:cloudformation:us-east-1:123456789:stack/MyStack/aaf549a0-a413-11df-adb3-5081b3858e83
-- MyStack MySG1 MyStack_SG1 AWS:: SecurityGroup 2010-07-27T22:28:28Z
-- CREATE_COMPLETE.
module Network.AWS.CloudFormation.DescribeStackEvents where

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
describeStackEvents :: DescribeStackEvents
describeStackEvents = DescribeStackEvents
    { dseiNextToken = Nothing
    , dseiStackName = Nothing
    }

data DescribeStackEvents = DescribeStackEvents
    { dseiNextToken :: Maybe Text
      -- ^ String that identifies the start of the next list of events, if there is
      -- one. Default: There is no default value.
    , dseiStackName :: Maybe Text
      -- ^ The name or the unique identifier associated with the stack. Default: There
      -- is no default value.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeStackEvents

instance AWSRequest DescribeStackEvents where
    type Er DescribeStackEvents = CloudFormationError
    type Rs DescribeStackEvents = DescribeStackEventsResponse
    request = getQuery service "DescribeStackEvents"

instance AWSPager DescribeStackEvents where
    next rq rs
        | Just x <- dseirsNextToken rs = Just $ rq { dseiNextToken = Just x }
        | otherwise = Nothing

data DescribeStackEventsResponse = DescribeStackEventsResponse
    { dseirsNextToken :: Maybe Text
      -- ^ String that identifies the start of the next list of events, if there is
      -- one.
    , dseirsStackEvents :: [StackEvent]
      -- ^ A list of StackEvents structures.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeStackEventsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeStackEventsResponse"
        :| ["DescribeStackEventsResult"]
