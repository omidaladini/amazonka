{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteCustomerGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a customer gateway. You must delete the VPN connection before
-- deleting the customer gateway. You can have a single active customer
-- gateway per AWS account (active means that you've created a VPN connection
-- with that customer gateway). AWS might delete any customer gateway you
-- leave inactive for an extended period of time.
module Network.AWS.EC2.DeleteCustomerGateway where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DeleteCustomerGateway = DeleteCustomerGateway
    { dcgsCustomerGatewayId :: !Text
      -- ^ The ID of the customer gateway to delete.
    , dcgsDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteCustomerGateway

instance AWSRequest DeleteCustomerGateway where
    type Er DeleteCustomerGateway = EC2Error
    type Rs DeleteCustomerGateway = DeleteCustomerGatewayResponse
    request = v2Query service GET "DeleteCustomerGateway"

data DeleteCustomerGatewayResponse = DeleteCustomerGatewayResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteCustomerGatewayResponse where
    fromXMLOptions = xmlOptions
