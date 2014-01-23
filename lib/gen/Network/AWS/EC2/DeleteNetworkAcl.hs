{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteNetworkAcl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a network ACL from a VPC. The ACL must not have any subnets
-- associated with it. You can't delete the default network ACL. For more
-- information about network ACLs, go to Network ACLs in the Amazon Virtual
-- Private Cloud User Guide.
module Network.AWS.EC2.DeleteNetworkAcl where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DeleteNetworkAcl = DeleteNetworkAcl
    { dnarDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , dnarNetworkAclId :: !Text
      -- ^ The ID of the network ACL to be deleted.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteNetworkAcl

instance AWSRequest DeleteNetworkAcl where
    type Er DeleteNetworkAcl = EC2Error
    type Rs DeleteNetworkAcl = DeleteNetworkAclResponse
    request = v2Query service GET "DeleteNetworkAcl"

data DeleteNetworkAclResponse = DeleteNetworkAclResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteNetworkAclResponse where
    fromXMLOptions = xmlOptions
