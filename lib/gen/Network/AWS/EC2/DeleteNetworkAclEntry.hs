{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteNetworkAclEntry
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes an ingress or egress entry (i.e., rule) from a network ACL. For
-- more information about network ACLs, go to Network ACLs in the Amazon
-- Virtual Private Cloud User Guide.
module Network.AWS.EC2.DeleteNetworkAclEntry where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteNetworkAclEntry :: Bool
                      -> Text
                      -> Int
                      -> DeleteNetworkAclEntry
deleteNetworkAclEntry p1 p2 p3 = undefined $ DeleteNetworkAclEntry
    { dnaerEgress = p1
    , dnaerNetworkAclId = p2
    , dnaerRuleNumber = p3
    , dnaerDryRun = Nothing
    }

data DeleteNetworkAclEntry = DeleteNetworkAclEntry
    { dnaerDryRun :: Maybe Bool
    , dnaerEgress :: !Bool
      -- ^ Whether the rule to delete is an egress rule (true) or ingress rule
      -- (false).
    , dnaerNetworkAclId :: !Text
      -- ^ ID of the network ACL.
    , dnaerRuleNumber :: !Int
      -- ^ Rule number for the entry to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteNetworkAclEntry

instance AWSRequest DeleteNetworkAclEntry where
    type Er DeleteNetworkAclEntry = EC2Error
    type Rs DeleteNetworkAclEntry = DeleteNetworkAclEntryResponse
    request = getQuery service "DeleteNetworkAclEntry"

data DeleteNetworkAclEntryResponse = DeleteNetworkAclEntryResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteNetworkAclEntryResponse where
    fromXMLOptions = xmlOptions
