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

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
cancelSpotInstanceRequests :: [Text]
                           -- ^ Specifies the ID of the Spot Instance request.
                           -> CancelSpotInstanceRequests
cancelSpotInstanceRequests p1 = CancelSpotInstanceRequests
    { csirSpotInstanceRequestIds = p1
    , csirDryRun = Nothing
    }

data CancelSpotInstanceRequests = CancelSpotInstanceRequests
    { csirDryRun :: Maybe Bool
    , csirSpotInstanceRequestIds :: [Text]
      -- ^ Specifies the ID of the Spot Instance request.
    } deriving (Eq, Show, Generic)

instance ToQuery CancelSpotInstanceRequests

instance AWSRequest CancelSpotInstanceRequests where
    type Er CancelSpotInstanceRequests = EC2Error
    type Rs CancelSpotInstanceRequests = CancelSpotInstanceRequestsResponse
    request = getQuery service "CancelSpotInstanceRequests"

data CancelSpotInstanceRequestsResponse = CancelSpotInstanceRequestsResponse
    { csirrCancelledSpotInstanceRequests :: [CancelledSpotInstanceRequest]
    } deriving (Eq, Show, Generic)

instance FromXML CancelSpotInstanceRequestsResponse where
    fromXMLOptions = xmlOptions
