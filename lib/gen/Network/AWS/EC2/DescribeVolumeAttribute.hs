{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeVolumeAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for DescribeVolumeAttribute
module Network.AWS.EC2.DescribeVolumeAttribute where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeVolumeAttribute :: Text
                        -> DescribeVolumeAttribute
describeVolumeAttribute p1 = DescribeVolumeAttribute
    { dvaVolumeId = p1
    , dvaAttribute = Nothing
    , dvaDryRun = Nothing
    }

data DescribeVolumeAttribute = DescribeVolumeAttribute
    { dvaAttribute :: Maybe VolumeAttributeName
    , dvaDryRun :: Maybe Bool
    , dvaVolumeId :: !Text
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeVolumeAttribute

instance AWSRequest DescribeVolumeAttribute where
    type Er DescribeVolumeAttribute = EC2Error
    type Rs DescribeVolumeAttribute = DescribeVolumeAttributeResponse
    request = getQuery service "DescribeVolumeAttribute"

data DescribeVolumeAttributeResponse = DescribeVolumeAttributeResponse
    { dvarAutoEnableIO :: Maybe AttributeBooleanValue
      -- ^ Boolean value.
    , dvarProductCodes :: [ProductCode]
    , dvarVolumeId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromXML DescribeVolumeAttributeResponse where
    fromXMLOptions = xmlOptions
