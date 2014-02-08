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
    { dniarNetworkInterfaceId = p1
    , dniarAttachment = Nothing
    , dniarDescription = Nothing
    , dniarDryRun = Nothing
    , dniarGroups = Nothing
    , dniarSourceDestCheck = Nothing
    }

data DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttribute
    { dniarAttachment :: Maybe Text
    , dniarDescription :: Maybe Text
    , dniarDryRun :: Maybe Bool
    , dniarGroups :: Maybe Text
    , dniarNetworkInterfaceId :: !Text
    , dniarSourceDestCheck :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeNetworkInterfaceAttribute

instance AWSRequest DescribeNetworkInterfaceAttribute where
    type Er DescribeNetworkInterfaceAttribute = EC2Error
    type Rs DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttributeResponse
    request = getQuery service "DescribeNetworkInterfaceAttribute"

data DescribeNetworkInterfaceAttributeResponse = DescribeNetworkInterfaceAttributeResponse
    { dniarrsAttachment :: Maybe NetworkInterfaceAttachment
    , dniarrsDescription :: Maybe AttributeValue
      -- ^ String value.
    , dniarrsGroups :: [GroupIdentifier]
    , dniarrsNetworkInterfaceId :: Maybe Text
    , dniarrsSourceDestCheck :: Maybe AttributeBooleanValue
      -- ^ Boolean value.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeNetworkInterfaceAttributeResponse where
    fromXMLOptions = xmlOptions
