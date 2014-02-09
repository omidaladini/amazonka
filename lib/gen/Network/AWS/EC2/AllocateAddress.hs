{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.AllocateAddress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The AllocateAddress operation acquires an elastic IP address for use with
-- your account.
module Network.AWS.EC2.AllocateAddress where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data AllocateAddress = AllocateAddress
    { aaDomain :: Maybe DomainType
      -- ^ Set to vpc to allocate the address to your VPC. By default, will allocate
      -- to EC2.
    , aaDryRun :: Maybe Bool
    } deriving (Eq, Show, Generic)

instance ToQuery AllocateAddress

instance AWSRequest AllocateAddress where
    type Er AllocateAddress = EC2Error
    type Rs AllocateAddress = AllocateAddressResponse
    request = getQuery service "AllocateAddress"

data AllocateAddressResponse = AllocateAddressResponse
    { aarAllocationId :: Maybe Text
    , aarDomain :: Maybe DomainType
    , aarPublicIp :: Maybe Text
      -- ^ IP address for use with your account.
    } deriving (Eq, Show, Generic)

instance FromXML AllocateAddressResponse where
    fromXMLOptions = xmlOptions
