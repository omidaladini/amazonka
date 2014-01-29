{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.DescribeTapeRecoveryPoints
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for DescribeTapeRecoveryPoints
module Network.AWS.StorageGateway.DescribeTapeRecoveryPoints where

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
describeTapeRecoveryPoints :: Text
                           -> DescribeTapeRecoveryPoints
describeTapeRecoveryPoints p1 = undefined $ DescribeTapeRecoveryPoints
    { dtrpiGatewayARN = p1
    , dtrpiLimit = Nothing
    , dtrpiMarker = Nothing
    }

data DescribeTapeRecoveryPoints = DescribeTapeRecoveryPoints
    { dtrpiGatewayARN :: !Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
      -- operation to return a list of gateways for your account and region.
    , dtrpiLimit :: Maybe Int
    , dtrpiMarker :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeTapeRecoveryPoints

instance AWSRequest DescribeTapeRecoveryPoints where
    type Er DescribeTapeRecoveryPoints = StorageGatewayError
    type Rs DescribeTapeRecoveryPoints = DescribeTapeRecoveryPointsResponse
    request  = getJSON service
    response = responseJSON

data DescribeTapeRecoveryPointsResponse = DescribeTapeRecoveryPointsResponse
    { dtrpirsGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
      -- operation to return a list of gateways for your account and region.
    , dtrpirsMarker :: Maybe Text
    , dtrpirsTapeRecoveryPointInfos :: [TapeRecoveryPointInfo]
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeTapeRecoveryPointsResponse
