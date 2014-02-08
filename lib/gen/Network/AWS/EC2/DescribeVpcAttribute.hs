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

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeVpcAttribute :: Text
                     -> DescribeVpcAttribute
describeVpcAttribute p1 = DescribeVpcAttribute
    { dvasVpcId = p1
    , dvasAttribute = Nothing
    , dvasDryRun = Nothing
    }

data DescribeVpcAttribute = DescribeVpcAttribute
    { dvasAttribute :: Maybe VpcAttributeName
    , dvasDryRun :: Maybe Bool
    , dvasVpcId :: !Text
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeVpcAttribute

instance AWSRequest DescribeVpcAttribute where
    type Er DescribeVpcAttribute = EC2Error
    type Rs DescribeVpcAttribute = DescribeVpcAttributeResponse
    request = getQuery service "DescribeVpcAttribute"

data DescribeVpcAttributeResponse = DescribeVpcAttributeResponse
    { dvasrsEnableDnsHostnames :: Maybe AttributeBooleanValue
      -- ^ Boolean value.
    , dvasrsEnableDnsSupport :: Maybe AttributeBooleanValue
      -- ^ Boolean value.
    , dvasrsVpcId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromXML DescribeVpcAttributeResponse where
    fromXMLOptions = xmlOptions
