{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.AttachInternetGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Attaches an Internet gateway to a VPC, enabling connectivity between the
-- Internet and the VPC. For more information about your VPC and Internet
-- gateway, go to the Amazon Virtual Private Cloud User Guide.
module Network.AWS.EC2.AttachInternetGateway where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
attachInternetGateway :: Text
                      -- ^ The ID of the Internet gateway to attach.
                      -> Text
                      -- ^ The ID of the VPC.
                      -> AttachInternetGateway
attachInternetGateway p1 p2 = AttachInternetGateway
    { aigInternetGatewayId = p1
    , aigVpcId = p2
    , aigDryRun = Nothing
    }

data AttachInternetGateway = AttachInternetGateway
    { aigDryRun :: Maybe Bool
    , aigInternetGatewayId :: !Text
      -- ^ The ID of the Internet gateway to attach.
    , aigVpcId :: !Text
      -- ^ The ID of the VPC.
    } deriving (Eq, Show, Generic)

instance ToQuery AttachInternetGateway

instance AWSRequest AttachInternetGateway where
    type Er AttachInternetGateway = EC2Error
    type Rs AttachInternetGateway = AttachInternetGatewayResponse
    request = getQuery service "AttachInternetGateway"

data AttachInternetGatewayResponse = AttachInternetGatewayResponse
    deriving (Eq, Show, Generic)

instance FromXML AttachInternetGatewayResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AttachInternetGatewayResponse"
