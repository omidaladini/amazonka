{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeNetworkInterfaceAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for DescribeNetworkInterfaceAttribute
module Network.AWS.EC2.DescribeNetworkInterfaceAttribute where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeNetworkInterfaceAttribute :: Text
                                  -> DescribeNetworkInterfaceAttribute
describeNetworkInterfaceAttribute p1 = DescribeNetworkInterfaceAttribute
    { dniaNetworkInterfaceId = p1
    , dniaAttachment = Nothing
    , dniaDescription = Nothing
    , dniaDryRun = Nothing
    , dniaGroups = Nothing
    , dniaSourceDestCheck = Nothing
    }

data DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttribute
    { dniaAttachment :: Maybe Text
    , dniaDescription :: Maybe Text
    , dniaDryRun :: Maybe Bool
    , dniaGroupSet :: Maybe Text
    , dniaNetworkInterfaceId :: !Text
    , dniaSourceDestCheck :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeNetworkInterfaceAttribute

instance AWSRequest DescribeNetworkInterfaceAttribute where
    type Er DescribeNetworkInterfaceAttribute = EC2Error
    type Rs DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttributeResponse
    request  = postQuery service "DescribeNetworkInterfaceAttribute"
    response = responseXML

data DescribeNetworkInterfaceAttributeResponse = DescribeNetworkInterfaceAttributeResponse
    { dniarAttachment :: Maybe NetworkInterfaceAttachment
    , dniarDescription :: Maybe AttributeValue
      -- ^ String value.
    , dniarGroupSet :: [GroupIdentifier]
    , dniarNetworkInterfaceId :: Maybe Text
    , dniarSourceDestCheck :: Maybe AttributeBooleanValue
      -- ^ Boolean value.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeNetworkInterfaceAttributeResponse where
    fromXMLOptions = xmlOptions
