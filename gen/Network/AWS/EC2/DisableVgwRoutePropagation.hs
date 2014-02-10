{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DisableVgwRoutePropagation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for DisableVgwRoutePropagation
module Network.AWS.EC2.DisableVgwRoutePropagation where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DisableVgwRoutePropagation = DisableVgwRoutePropagation
    { dvrpGatewayId :: !Text
    , dvrpRouteTableId :: !Text
    } deriving (Eq, Show, Generic)

instance ToQuery DisableVgwRoutePropagation

instance AWSRequest DisableVgwRoutePropagation where
    type Er DisableVgwRoutePropagation = EC2Error
    type Rs DisableVgwRoutePropagation = DisableVgwRoutePropagationResponse
    request  = postQuery service "DisableVgwRoutePropagation"
    response = responseXML

data DisableVgwRoutePropagationResponse = DisableVgwRoutePropagationResponse
    deriving (Eq, Show, Generic)

instance FromXML DisableVgwRoutePropagationResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DisableVgwRoutePropagationResponse"
