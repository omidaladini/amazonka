{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateInternetGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new Internet gateway in your AWS account. After creating the
-- Internet gateway, you then attach it to a VPC using AttachInternetGateway.
-- For more information about your VPC and Internet gateway, go to Amazon
-- Virtual Private Cloud User Guide.
module Network.AWS.EC2.CreateInternetGateway where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data CreateInternetGateway = CreateInternetGateway
    { cigDryRun :: Maybe Bool
    } deriving (Eq, Show, Generic)

instance ToQuery CreateInternetGateway

instance AWSRequest CreateInternetGateway where
    type Er CreateInternetGateway = EC2Error
    type Rs CreateInternetGateway = CreateInternetGatewayResponse
    request  = postQuery service "CreateInternetGateway"
    response = responseXML

data CreateInternetGatewayResponse = CreateInternetGatewayResponse
    { cigrInternetGateway :: Maybe InternetGateway
    } deriving (Eq, Show, Generic)

instance FromXML CreateInternetGatewayResponse where
    fromXMLOptions = xmlOptions
