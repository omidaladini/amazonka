{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteSubnet
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a subnet from a VPC. You must terminate all running instances in
-- the subnet before deleting it, otherwise Amazon VPC returns an error.
module Network.AWS.EC2.DeleteSubnet where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteSubnet :: Text
             -- ^ The ID of the subnet you want to delete.
             -> DeleteSubnet
deleteSubnet p1 = DeleteSubnet
    { dseSubnetId = p1
    , dseDryRun = Nothing
    }

data DeleteSubnet = DeleteSubnet
    { dseDryRun :: Maybe Bool
    , dseSubnetId :: !Text
      -- ^ The ID of the subnet you want to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteSubnet

instance AWSRequest DeleteSubnet where
    type Er DeleteSubnet = EC2Error
    type Rs DeleteSubnet = DeleteSubnetResponse
    request  = postQuery service "DeleteSubnet"
    response = responseXML

data DeleteSubnetResponse = DeleteSubnetResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteSubnetResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteSubnetResponse"
