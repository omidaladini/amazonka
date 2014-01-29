{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.DeleteTape
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for DeleteTape
module Network.AWS.StorageGateway.DeleteTape where

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
deleteTape :: Text
           -> Text
           -> DeleteTape
deleteTape p1 p2 = undefined $ DeleteTape
    { dtjGatewayARN = p1
    , dtjTapeARN = p2
    }

data DeleteTape = DeleteTape
    { dtjGatewayARN :: !Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
      -- operation to return a list of gateways for your account and region.
    , dtjTapeARN :: !Text
    } deriving (Eq, Show, Generic)

instance ToJSON DeleteTape

instance AWSRequest DeleteTape where
    type Er DeleteTape = StorageGatewayError
    type Rs DeleteTape = DeleteTapeResponse
    request  = getJSON service
    response = responseJSON

data DeleteTapeResponse = DeleteTapeResponse
    { dtjrsTapeARN :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromJSON DeleteTapeResponse
