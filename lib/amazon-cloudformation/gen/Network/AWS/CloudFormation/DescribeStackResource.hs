{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.DescribeStackResource
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a description of the specified resource in the specified stack. For
-- deleted stacks, DescribeStackResource returns resource information for up
-- to 90 days after the stack has been deleted.
-- https://cloudformation.us-east-1.amazonaws.com/
-- ?Action=DescribeStackResource &StackName=MyStack
-- &LogicalResourceId=MyDBInstance &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2011-07-08T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature]
-- arn:aws:cloudformation:us-east-1:123456789:stack/MyStack/aaf549a0-a413-11df-adb3-5081b3858e83
-- MyStack MyDBInstance MyStack_DB1 AWS::RDS::DBInstance 2011-07-07T22:27:28Z
-- CREATE_COMPLETE.
module Network.AWS.CloudFormation.DescribeStackResource where

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
describeStackResource :: Text
                      -> Text
                      -> DescribeStackResource
describeStackResource p1 p2 = DescribeStackResource
    { dsrjLogicalResourceId = p1
    , dsrjStackName = p2
    }

data DescribeStackResource = DescribeStackResource
    { dsrjLogicalResourceId :: !Text
      -- ^ The logical name of the resource as specified in the template. Default:
      -- There is no default value.
    , dsrjStackName :: !Text
      -- ^ The name or the unique identifier associated with the stack. Default: There
      -- is no default value.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeStackResource

instance AWSRequest DescribeStackResource where
    type Er DescribeStackResource = CloudFormationError
    type Rs DescribeStackResource = DescribeStackResourceResponse
    request = getQuery service "DescribeStackResource"

data DescribeStackResourceResponse = DescribeStackResourceResponse
    { dsrjrsStackResourceDetail :: Maybe StackResourceDetail
      -- ^ A StackResourceDetail structure containing the description of the specified
      -- resource in the specified stack.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeStackResourceResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeStackResourceResponse"
        :| ["DescribeStackResourceResult"]
