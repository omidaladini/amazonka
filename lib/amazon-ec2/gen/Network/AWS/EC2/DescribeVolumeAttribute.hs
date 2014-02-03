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

import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeVolumeAttribute :: Text
                        -> DescribeVolumeAttribute
describeVolumeAttribute p1 = DescribeVolumeAttribute
    { dvarVolumeId = p1
    , dvarAttribute = Nothing
    , dvarDryRun = Nothing
    }

data DescribeVolumeAttribute = DescribeVolumeAttribute
    { dvarAttribute :: Maybe VolumeAttributeName
    , dvarDryRun :: Maybe Bool
    , dvarVolumeId :: !Text
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeVolumeAttribute

instance AWSRequest DescribeVolumeAttribute where
    type Er DescribeVolumeAttribute = EC2Error
    type Rs DescribeVolumeAttribute = DescribeVolumeAttributeResponse
    request = getQuery service "DescribeVolumeAttribute"

data DescribeVolumeAttributeResponse = DescribeVolumeAttributeResponse
    { dvarrsAutoEnableIO :: Maybe AttributeBooleanValue
      -- ^ Boolean value.
    , dvarrsProductCodes :: [ProductCode]
    , dvarrsVolumeId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromXML DescribeVolumeAttributeResponse where
    fromXMLOptions = xmlOptions
