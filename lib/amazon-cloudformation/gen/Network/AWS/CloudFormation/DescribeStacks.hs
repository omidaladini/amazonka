{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.DescribeStacks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the description for the specified stack; if no stack name was
-- specified, then it returns the description for all the stacks created.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=DescribeStacks
-- &StackName=MyStack &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature] MyStack
-- arn:aws:cloudformation:us-east-1:123456789:stack/MyStack/aaf549a0-a413-11df-adb3-5081b3858e83
-- 2010-07-27T22:28:28Z CREATE_COMPLETE false StartPage
-- http://my-load-balancer.amazonaws.com:80/index.html.
module Network.AWS.CloudFormation.DescribeStacks where

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
describeStacks :: DescribeStacks
describeStacks = DescribeStacks
    { dsjNextToken = Nothing
    , dsjStackName = Nothing
    }

data DescribeStacks = DescribeStacks
    { dsjNextToken :: Maybe Text
    , dsjStackName :: Maybe Text
      -- ^ The name or the unique identifier associated with the stack. Default: There
      -- is no default value.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeStacks

instance AWSRequest DescribeStacks where
    type Er DescribeStacks = CloudFormationError
    type Rs DescribeStacks = DescribeStacksResponse
    request = getQuery service "DescribeStacks"

instance AWSPager DescribeStacks where
    next rq rs
        | Just x <- dsjrsNextToken rs = Just $ rq { dsjNextToken = Just x }
        | otherwise = Nothing

data DescribeStacksResponse = DescribeStacksResponse
    { dsjrsNextToken :: Maybe Text
    , dsjrsStacks :: [Stack]
      -- ^ A list of stack structures.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeStacksResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeStacksResponse"
        :| ["DescribeStacksResult"]
