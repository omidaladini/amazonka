{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeVpcAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for DescribeVpcAttribute
module Network.AWS.EC2.DescribeVpcAttribute where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeVpcAttribute = DescribeVpcAttribute
    { dvasAttribute :: Maybe VpcAttributeName
      -- ^ FIXME: Missing documentation
    , dvasDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , dvasVpcId :: !Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeVpcAttribute

instance AWSRequest DescribeVpcAttribute where
    type Er DescribeVpcAttribute = EC2Error
    type Rs DescribeVpcAttribute = DescribeVpcAttributeResponse
    request = v2Query service GET "DescribeVpcAttribute"

data DescribeVpcAttributeResponse = DescribeVpcAttributeResponse
    { dvasrsEnableDnsHostnames :: Maybe AttributeBooleanValue
      -- ^ Boolean value.
    , dvasrsEnableDnsSupport :: Maybe AttributeBooleanValue
      -- ^ Boolean value.
    , dvasrsVpcId :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance FromXML DescribeVpcAttributeResponse where
    fromXMLOptions = xmlOptions
