{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CancelSpotInstanceRequests
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Cancels one or more Spot Instance requests. Spot Instances are instances
-- that Amazon EC2 starts on your behalf when the maximum price that you
-- specify exceeds the current Spot Price. Amazon EC2 periodically sets the
-- Spot Price based on available Spot Instance capacity and current spot
-- instance requests. For conceptual information about Spot Instances, refer
-- to the Amazon Elastic Compute Cloud Developer Guide or Amazon Elastic
-- Compute Cloud User Guide .
module Network.AWS.EC2.CancelSpotInstanceRequests where

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

-- | Convenience method utilising default fields where applicable.
cancelSpotInstanceRequests :: [Text]
                           -> AWS (Either EC2Error CancelSpotInstanceRequestsResponse)
cancelSpotInstanceRequests p1 = undefined $ CancelSpotInstanceRequests
    { csirrSpotInstanceRequestIds = p1
    , csirrDryRun = Nothing
    }

data CancelSpotInstanceRequests = CancelSpotInstanceRequests
    { csirrDryRun :: Maybe Bool
    , csirrSpotInstanceRequestIds :: [Text]
      -- ^ Specifies the ID of the Spot Instance request.
    } deriving (Eq, Show, Generic)

instance ToQuery CancelSpotInstanceRequests

instance AWSRequest CancelSpotInstanceRequests where
    type Er CancelSpotInstanceRequests = EC2Error
    type Rs CancelSpotInstanceRequests = CancelSpotInstanceRequestsResponse
    request = getQuery service "CancelSpotInstanceRequests"

data CancelSpotInstanceRequestsResponse = CancelSpotInstanceRequestsResponse
    { csirrrsCancelledSpotInstanceRequests :: [CancelledSpotInstanceRequest]
    } deriving (Eq, Show, Generic)

instance FromXML CancelSpotInstanceRequestsResponse where
    fromXMLOptions = xmlOptions
