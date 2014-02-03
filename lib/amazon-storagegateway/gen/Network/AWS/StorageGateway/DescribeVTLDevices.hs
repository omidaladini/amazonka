{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.DescribeVTLDevices
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for DescribeVTLDevices
module Network.AWS.StorageGateway.DescribeVTLDevices where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.StorageGateway.Service
import Network.AWS.StorageGateway.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeVTLDevices :: Text
                   -> DescribeVTLDevices
describeVTLDevices p1 = DescribeVTLDevices
    { dvtldiGatewayARN = p1
    , dvtldiLimit = Nothing
    , dvtldiMarker = Nothing
    , dvtldiVTLDeviceARNs = []
    }

data DescribeVTLDevices = DescribeVTLDevices
    { dvtldiGatewayARN :: !Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
      -- operation to return a list of gateways for your account and region.
    , dvtldiLimit :: Maybe Int
    , dvtldiMarker :: Maybe Text
    , dvtldiVTLDeviceARNs :: [Text]
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeVTLDevices where
    toJSON = genericToJSON jsonOptions

instance AWSRequest DescribeVTLDevices where
    type Er DescribeVTLDevices = StorageGatewayError
    type Rs DescribeVTLDevices = DescribeVTLDevicesResponse
    request  = getJSON service
    response = responseJSON

data DescribeVTLDevicesResponse = DescribeVTLDevicesResponse
    { dvtldirsGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
      -- operation to return a list of gateways for your account and region.
    , dvtldirsMarker :: Maybe Text
    , dvtldirsVTLDevices :: [VTLDevice]
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeVTLDevicesResponse where
    fromJSON = genericFromJSON jsonOptions

