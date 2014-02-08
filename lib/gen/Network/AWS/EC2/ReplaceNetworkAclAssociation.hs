{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ReplaceNetworkAclAssociation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Changes which network ACL a subnet is associated with. By default when you
-- create a subnet, it's automatically associated with the default network
-- ACL. For more information about network ACLs, go to Network ACLs in the
-- Amazon Virtual Private Cloud User Guide.
module Network.AWS.EC2.ReplaceNetworkAclAssociation where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
replaceNetworkAclAssociation :: Text
                             -- ^ The ID representing the current association between the original network
                             -- ACL and the subnet.
                             -> Text
                             -- ^ The ID of the new ACL to associate with the subnet.
                             -> ReplaceNetworkAclAssociation
replaceNetworkAclAssociation p1 p2 = ReplaceNetworkAclAssociation
    { rnaarAssociationId = p1
    , rnaarNetworkAclId = p2
    , rnaarDryRun = Nothing
    }

data ReplaceNetworkAclAssociation = ReplaceNetworkAclAssociation
    { rnaarAssociationId :: !Text
      -- ^ The ID representing the current association between the original network
      -- ACL and the subnet.
    , rnaarDryRun :: Maybe Bool
    , rnaarNetworkAclId :: !Text
      -- ^ The ID of the new ACL to associate with the subnet.
    } deriving (Eq, Show, Generic)

instance ToQuery ReplaceNetworkAclAssociation

instance AWSRequest ReplaceNetworkAclAssociation where
    type Er ReplaceNetworkAclAssociation = EC2Error
    type Rs ReplaceNetworkAclAssociation = ReplaceNetworkAclAssociationResponse
    request = getQuery service "ReplaceNetworkAclAssociation"

data ReplaceNetworkAclAssociationResponse = ReplaceNetworkAclAssociationResponse
    { rnaarrsNewAssociationId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromXML ReplaceNetworkAclAssociationResponse where
    fromXMLOptions = xmlOptions
