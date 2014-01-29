{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DeleteDBSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a DB subnet group. The specified database subnet group must not be
-- associated with any DB instances. https://rds.amazonaws.com/
-- ?Action=DeleteDBSubnetGroup &DBSubnetGroupName=mysubnetgroup
-- &Version=2013-05-15 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-02-15T17%3A48%3A21.746Z &AWSAccessKeyId= &Signature=
-- 5d013245-4172-11df-8520-e7e1e602a915.
module Network.AWS.RDS.DeleteDBSubnetGroup where

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

import Network.AWS.RDS.Service
import Network.AWS.RDS.Types

-- | Convenience method utilising default fields where applicable.
deleteDBSubnetGroup :: Text
                    -> AWS (Either RDSError DeleteDBSubnetGroupResponse)
deleteDBSubnetGroup p1 = undefined $ DeleteDBSubnetGroup
    { ddbsgnDBSubnetGroupName = p1
    }

data DeleteDBSubnetGroup = DeleteDBSubnetGroup
    { ddbsgnDBSubnetGroupName :: !Text
      -- ^ The name of the database subnet group to delete. You cannot delete the
      -- default subnet group. Constraints: Must be 1 to 255 alphanumeric characters
      -- First character must be a letter Cannot end with a hyphen or contain two
      -- consecutive hyphens.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteDBSubnetGroup

instance AWSRequest DeleteDBSubnetGroup where
    type Er DeleteDBSubnetGroup = RDSError
    type Rs DeleteDBSubnetGroup = DeleteDBSubnetGroupResponse
    request = getQuery service "DeleteDBSubnetGroup"

data DeleteDBSubnetGroupResponse = DeleteDBSubnetGroupResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteDBSubnetGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeleteDBSubnetGroupResponse"
        :| ["DeleteDBSubnetGroupResult"]
