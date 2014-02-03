{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.CancelRetrieval
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for CancelRetrieval
module Network.AWS.StorageGateway.CancelRetrieval where

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
cancelRetrieval :: Text
                -> Text
                -> CancelRetrieval
cancelRetrieval p1 p2 = CancelRetrieval
    { criGatewayARN = p1
    , criTapeARN = p2
    }

data CancelRetrieval = CancelRetrieval
    { criGatewayARN :: !Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
      -- operation to return a list of gateways for your account and region.
    , criTapeARN :: !Text
    } deriving (Eq, Show, Generic)

instance ToJSON CancelRetrieval where
    toJSON = genericToJSON jsonOptions

instance AWSRequest CancelRetrieval where
    type Er CancelRetrieval = StorageGatewayError
    type Rs CancelRetrieval = CancelRetrievalResponse
    request  = getJSON service
    response = responseJSON

data CancelRetrievalResponse = CancelRetrievalResponse
    { crirsTapeARN :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromJSON CancelRetrievalResponse where
    fromJSON = genericFromJSON jsonOptions

