{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.DescribeTapes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for DescribeTapes
module Network.AWS.StorageGateway.DescribeTapes where

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
describeTapes :: Text
              -> DescribeTapes
describeTapes p1 = DescribeTapes
    { dtiGatewayARN = p1
    , dtiLimit = Nothing
    , dtiMarker = Nothing
    , dtiTapeARNs = []
    }

data DescribeTapes = DescribeTapes
    { dtiGatewayARN :: !Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
      -- operation to return a list of gateways for your account and region.
    , dtiLimit :: Maybe Int
    , dtiMarker :: Maybe Text
    , dtiTapeARNs :: [Text]
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeTapes where
    toJSON = genericToJSON jsonOptions

instance AWSRequest DescribeTapes where
    type Er DescribeTapes = StorageGatewayError
    type Rs DescribeTapes = DescribeTapesResponse
    request  = getJSON service
    response = responseJSON

data DescribeTapesResponse = DescribeTapesResponse
    { dtirsMarker :: Maybe Text
    , dtirsTapes :: [Tape]
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeTapesResponse where
    fromJSON = genericFromJSON jsonOptions

