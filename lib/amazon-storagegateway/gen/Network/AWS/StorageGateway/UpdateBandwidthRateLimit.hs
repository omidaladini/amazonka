{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.UpdateBandwidthRateLimit
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation updates the bandwidth rate limits of a gateway. You can
-- update both the upload and download bandwidth rate limit or specify only
-- one of the two. If you don't set a bandwidth rate limit, the existing rate
-- limit remains. By default, a gateway's bandwidth rate limits are not set.
-- If you don't set any limit, the gateway does not have any limitations on
-- its bandwidth usage and could potentially use the maximum available
-- bandwidth. To specify which gateway to update, use the Amazon Resource Name
-- (ARN) of the gateway in your request. Example Request The following example
-- shows a request that returns the bandwidth throttle properties of a
-- gateway. POST / HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com
-- x-amz-Date: 20120425T120000Z Authorization:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Content-type:
-- application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.UpdateBandwidthRateLimit { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "AverageUploadRateLimitInBitsPerSec": 51200,
-- "AverageDownloadRateLimitInBitsPerSec": 102400 } HTTP/1.1 200 OK
-- x-amzn-RequestId: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Date: Wed, 25 Apr 2012 12:00:02 GMT Content-type:
-- application/x-amz-json-1.1 Content-length: 80 { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }.
module Network.AWS.StorageGateway.UpdateBandwidthRateLimit where

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
updateBandwidthRateLimit :: Text
                         -> UpdateBandwidthRateLimit
updateBandwidthRateLimit p1 = UpdateBandwidthRateLimit
    { ubrliGatewayARN = p1
    , ubrliAverageDownloadRateLimitInBitsPerSec = Nothing
    , ubrliAverageUploadRateLimitInBitsPerSec = Nothing
    }

data UpdateBandwidthRateLimit = UpdateBandwidthRateLimit
    { ubrliAverageDownloadRateLimitInBitsPerSec :: Maybe Integer
      -- ^ The average download bandwidth rate limit in bits per second.
    , ubrliAverageUploadRateLimitInBitsPerSec :: Maybe Integer
      -- ^ The average upload bandwidth rate limit in bits per second.
    , ubrliGatewayARN :: !Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
      -- operation to return a list of gateways for your account and region.
    } deriving (Eq, Show, Generic)

instance ToJSON UpdateBandwidthRateLimit where
    toJSON = genericToJSON jsonOptions

instance AWSRequest UpdateBandwidthRateLimit where
    type Er UpdateBandwidthRateLimit = StorageGatewayError
    type Rs UpdateBandwidthRateLimit = UpdateBandwidthRateLimitResponse
    request  = getJSON service
    response = responseJSON

data UpdateBandwidthRateLimitResponse = UpdateBandwidthRateLimitResponse
    { ubrlirsGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
      -- operation to return a list of gateways for your account and region.
    } deriving (Eq, Show, Generic)

instance FromJSON UpdateBandwidthRateLimitResponse where
    fromJSON = genericFromJSON jsonOptions

