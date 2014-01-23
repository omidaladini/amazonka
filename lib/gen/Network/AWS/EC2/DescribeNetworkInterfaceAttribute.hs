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

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttribute
    { dniarAttachment :: Maybe Text
      -- ^ FIXME: Missing documentation
    , dniarDescription :: Maybe Text
      -- ^ FIXME: Missing documentation
    , dniarDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , dniarGroups :: Maybe Text
      -- ^ FIXME: Missing documentation
    , dniarNetworkInterfaceId :: !Text
      -- ^ FIXME: Missing documentation
    , dniarSourceDestCheck :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeNetworkInterfaceAttribute

instance AWSRequest DescribeNetworkInterfaceAttribute where
    type Er DescribeNetworkInterfaceAttribute = EC2Error
    type Rs DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttributeResponse
    request = v2Query service GET "DescribeNetworkInterfaceAttribute"

data DescribeNetworkInterfaceAttributeResponse = DescribeNetworkInterfaceAttributeResponse
    { dniarrsAttachment :: Maybe NetworkInterfaceAttachment
      -- ^ FIXME: Missing documentation
    , dniarrsDescription :: Maybe AttributeValue
      -- ^ String value.
    , dniarrsGroups :: [GroupIdentifier]
      -- ^ FIXME: Missing documentation
    , dniarrsNetworkInterfaceId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , dniarrsSourceDestCheck :: Maybe AttributeBooleanValue
      -- ^ Boolean value.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeNetworkInterfaceAttributeResponse where
    fromXMLOptions = xmlOptions
