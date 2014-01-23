{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DisassociateAddress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DisassociateAddress operation disassociates the specified elastic IP
-- address from the instance to which it is assigned. This is an idempotent
-- operation. If you enter it more than once, Amazon EC2 does not return an
-- error.
module Network.AWS.EC2.DisassociateAddress where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DisassociateAddress = DisassociateAddress
    { dasAssociationId :: Maybe Text
      -- ^ Association ID corresponding to the VPC elastic IP address you want to
      -- disassociate.
    , dasDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , dasPublicIp :: Maybe Text
      -- ^ The elastic IP address that you are disassociating from the instance.
    } deriving (Eq, Show, Generic)

instance ToQuery DisassociateAddress

instance AWSRequest DisassociateAddress where
    type Er DisassociateAddress = EC2Error
    type Rs DisassociateAddress = DisassociateAddressResponse
    request = v2Query service GET "DisassociateAddress"

data DisassociateAddressResponse = DisassociateAddressResponse
    deriving (Eq, Show, Generic)

instance FromXML DisassociateAddressResponse where
    fromXMLOptions = xmlOptions
