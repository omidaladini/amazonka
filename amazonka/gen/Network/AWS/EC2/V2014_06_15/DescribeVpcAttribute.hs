{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeVpcAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the specified attribute of the specified VPC. You can specify
-- only one attribute at a time. Example 1 This example describes the
-- enableDnsSupport attribute of the specified VPC. The sample response
-- indicates that DNS resolution is supported.
-- https://ec2.amazonaws.com/?Action=DescribeVpcAttribute
-- &amp;VpcId=vpc-1a2b3c4d &amp;Attribute=enableDnsSupport &amp;AUTHPARAMS
-- &lt;DescribeVpcAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;vpcId&gt;vpc-1a2b3c4d&lt;/vpcId&gt; &lt;enableDnsSupport&gt;
-- &lt;value&gt;true&lt;/value&gt; &lt;/enableDnsSupport&gt;
-- &lt;/DescribeVpcAttributeResponse&gt; Example 2 This request describes the
-- enableDnsHostnames attribute of the specified VPC. The sample response
-- indicates that DNS hostnames are supported.
-- https://ec2.amazonaws.com/?Action=DescribeVpcAttribute
-- &amp;VpcId=vpc-1a2b3c4d &amp;Attribute=enableDnsHostnames &amp;AUTHPARAMS
-- &lt;DescribeVpcAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;vpcId&gt;vpc-1a2b3c4d&lt;/vpcId&gt; &lt;enableDnsHostnames&gt;
-- &lt;value&gt;true&lt;/value&gt; &lt;/enableDnsHostnames&gt;
-- &lt;/DescribeVpcAttributeResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DescribeVpcAttribute where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeVpcAttribute' request.
describeVpcAttribute :: Text -- ^ '_dvatVpcId'
                     -> DescribeVpcAttribute
describeVpcAttribute p1 = DescribeVpcAttribute
    { _dvatVpcId = p1
    , _dvatDryRun = Nothing
    , _dvatAttribute = Nothing
    }

data DescribeVpcAttribute = DescribeVpcAttribute
    { _dvatVpcId :: Text
      -- ^ The ID of the VPC.
    , _dvatDryRun :: Maybe Bool
      -- ^ 
    , _dvatAttribute :: Maybe VpcAttributeName
      -- ^ The VPC attribute.
    } deriving (Generic)

makeLenses ''DescribeVpcAttribute

instance ToQuery DescribeVpcAttribute where
    toQuery = genericToQuery def

data DescribeVpcAttributeResponse = DescribeVpcAttributeResponse
    { _dvauEnableDnsHostnames :: Maybe AttributeBooleanValue
      -- ^ Indicates whether the instances launched in the VPC get DNS
      -- hostnames. If this attribute is true, instances in the VPC get
      -- DNS hostnames; otherwise, they do not.
    , _dvauEnableDnsSupport :: Maybe AttributeBooleanValue
      -- ^ Indicates whether DNS resolution is enabled for the VPC. If this
      -- attribute is true, the Amazon DNS server resolves DNS hostnames
      -- for your instances to their corresponding IP addresses;
      -- otherwise, it does not.
    , _dvauVpcId :: Maybe Text
      -- ^ The ID of the VPC.
    } deriving (Generic)

makeLenses ''DescribeVpcAttributeResponse

instance FromXML DescribeVpcAttributeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeVpcAttribute where
    type Sv DescribeVpcAttribute = EC2
    type Rs DescribeVpcAttribute = DescribeVpcAttributeResponse

    request = post "DescribeVpcAttribute"
    response _ = xmlResponse