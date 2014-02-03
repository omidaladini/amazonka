{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.CreateTapes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for CreateTapes
module Network.AWS.StorageGateway.CreateTapes where

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
createTapes :: Text
            -> Text
            -> Int
            -> Text
            -> Integer
            -> CreateTapes
createTapes p1 p2 p3 p4 p5 = CreateTapes
    { ctiClientToken = p1
    , ctiGatewayARN = p2
    , ctiNumTapesToCreate = p3
    , ctiTapeBarcodePrefix = p4
    , ctiTapeSizeInBytes = p5
    }

data CreateTapes = CreateTapes
    { ctiClientToken :: !Text
    , ctiGatewayARN :: !Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
      -- operation to return a list of gateways for your account and region.
    , ctiNumTapesToCreate :: !Int
    , ctiTapeBarcodePrefix :: !Text
    , ctiTapeSizeInBytes :: !Integer
    } deriving (Eq, Show, Generic)

instance ToJSON CreateTapes where
    toJSON = genericToJSON jsonOptions

instance AWSRequest CreateTapes where
    type Er CreateTapes = StorageGatewayError
    type Rs CreateTapes = CreateTapesResponse
    request  = getJSON service
    response = responseJSON

data CreateTapesResponse = CreateTapesResponse
    { ctirsTapeARNs :: [Text]
    } deriving (Eq, Show, Generic)

instance FromJSON CreateTapesResponse where
    fromJSON = genericFromJSON jsonOptions

