{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteVpc
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a VPC. You must detach or delete all gateways or other objects that
-- are dependent on the VPC first. For example, you must terminate all running
-- instances, delete all VPC security groups (except the default), delete all
-- the route tables (except the default), etc.
module Network.AWS.EC2.DeleteVpc where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteVpc :: Text
          -- ^ The ID of the VPC you want to delete.
          -> DeleteVpc
deleteVpc p1 = DeleteVpc
    { dvfVpcId = p1
    , dvfDryRun = Nothing
    }

data DeleteVpc = DeleteVpc
    { dvfDryRun :: Maybe Bool
    , dvfVpcId :: !Text
      -- ^ The ID of the VPC you want to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteVpc

instance AWSRequest DeleteVpc where
    type Er DeleteVpc = EC2Error
    type Rs DeleteVpc = DeleteVpcResponse
    request  = postQuery service "DeleteVpc"
    response = responseXML

data DeleteVpcResponse = DeleteVpcResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteVpcResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteVpcResponse"