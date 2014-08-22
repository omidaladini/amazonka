{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DeleteVpcPeeringConnection
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a VPC peering connection. Either the owner of the requester VPC or
-- the owner of the peer VPC can delete the VPC peering connection if it's in
-- the active state. The owner of the requester VPC can delete a VPC peering
-- connection in the pending-acceptance state. Example This example deletes
-- the specified VPC peering connection.
-- https://ec2.amazonaws.com/?Action=DeleteVpcPeeringConnection
-- &amp;vpcPeeringConnectionId=pcx-1a2b3c4d &amp;AUTHPARAMS
-- &lt;DeleteVpcPeeringConnectionResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/DeleteVpcPeeringConnectionResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DeleteVpcPeeringConnection where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteVpcPeeringConnection' request.
deleteVpcPeeringConnection :: Text -- ^ '_dvpcrVpcPeeringConnectionId'
                           -> DeleteVpcPeeringConnection
deleteVpcPeeringConnection p1 = DeleteVpcPeeringConnection
    { _dvpcrVpcPeeringConnectionId = p1
    , _dvpcrDryRun = Nothing
    }

data DeleteVpcPeeringConnection = DeleteVpcPeeringConnection
    { _dvpcrVpcPeeringConnectionId :: Text
      -- ^ The ID of the VPC peering connection.
    , _dvpcrDryRun :: Maybe Bool
      -- ^ 
    } deriving (Show, Generic)

makeLenses ''DeleteVpcPeeringConnection

instance ToQuery DeleteVpcPeeringConnection where
    toQuery = genericQuery def

data DeleteVpcPeeringConnectionResponse = DeleteVpcPeeringConnectionResponse
    { _dvpcsReturn :: Maybe Bool
      -- ^ Returns true if the request succeeds; otherwise, it returns an
      -- error.
    } deriving (Show, Generic)

makeLenses ''DeleteVpcPeeringConnectionResponse

instance FromXML DeleteVpcPeeringConnectionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteVpcPeeringConnection where
    type Sv DeleteVpcPeeringConnection = EC2
    type Rs DeleteVpcPeeringConnection = DeleteVpcPeeringConnectionResponse

    request = post "DeleteVpcPeeringConnection"
    response _ = xmlResponse