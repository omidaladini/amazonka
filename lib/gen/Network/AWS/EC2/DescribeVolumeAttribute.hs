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

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeVolumeAttribute = DescribeVolumeAttribute
    { dvarAttribute :: Maybe VolumeAttributeName
      -- ^ FIXME: Missing documentation
    , dvarDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , dvarVolumeId :: !Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeVolumeAttribute

instance AWSRequest DescribeVolumeAttribute where
    type Er DescribeVolumeAttribute = EC2Error
    type Rs DescribeVolumeAttribute = DescribeVolumeAttributeResponse
    request = v2Query service GET "DescribeVolumeAttribute"

data DescribeVolumeAttributeResponse = DescribeVolumeAttributeResponse
    { dvarrsAutoEnableIO :: Maybe AttributeBooleanValue
      -- ^ Boolean value.
    , dvarrsProductCodes :: [ProductCode]
      -- ^ FIXME: Missing documentation
    , dvarrsVolumeId :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance FromXML DescribeVolumeAttributeResponse where
    fromXMLOptions = xmlOptions
