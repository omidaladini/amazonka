{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateNetworkAcl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new network ACL in a VPC. Network ACLs provide an optional layer
-- of security (on top of security groups) for the instances in your VPC. For
-- more information about network ACLs, go to Network ACLs in the Amazon
-- Virtual Private Cloud User Guide.
module Network.AWS.EC2.CreateNetworkAcl where

import           Control.Monad
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import           Text.XML.Generic

import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields where applicable.
createNetworkAcl :: Text
                 -> AWS (Either EC2Error CreateNetworkAclResponse)
createNetworkAcl p1 = undefined $ CreateNetworkAcl
    { cnarVpcId = p1
    , cnarDryRun = Nothing
    }

data CreateNetworkAcl = CreateNetworkAcl
    { cnarDryRun :: Maybe Bool
    , cnarVpcId :: !Text
      -- ^ The ID of the VPC where the network ACL will be created.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateNetworkAcl

instance AWSRequest CreateNetworkAcl where
    type Er CreateNetworkAcl = EC2Error
    type Rs CreateNetworkAcl = CreateNetworkAclResponse
    request = getQuery service "CreateNetworkAcl"

data CreateNetworkAclResponse = CreateNetworkAclResponse
    { cnarrsNetworkAcl :: Maybe NetworkAcl
    } deriving (Eq, Show, Generic)

instance FromXML CreateNetworkAclResponse where
    fromXMLOptions = xmlOptions
