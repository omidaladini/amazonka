{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DeleteDBSecurityGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a DB security group. The specified DB security group must not be
-- associated with any DB instances. https://rds.amazonaws.com/
-- ?Action=DeleteDBSecurityGroup &DBSecurityGroupName=mysecuritygroup
-- &Version=2013-05-15 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-02-15T17%3A48%3A21.746Z &AWSAccessKeyId= &Signature=
-- 5d013245-4172-11df-8520-e7e1e602a915.
module Network.AWS.RDS.DeleteDBSecurityGroup where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteDBSecurityGroup :: Text
                      -> DeleteDBSecurityGroup
deleteDBSecurityGroup p1 = DeleteDBSecurityGroup
    { ddbsgoDBSecurityGroupName = p1
    }

data DeleteDBSecurityGroup = DeleteDBSecurityGroup
    { ddbsgoDBSecurityGroupName :: !Text
      -- ^ The name of the DB security group to delete. You cannot delete the default
      -- DB security group. Constraints: Must be 1 to 255 alphanumeric characters
      -- First character must be a letter Cannot end with a hyphen or contain two
      -- consecutive hyphens Must not be "Default" May not contain spaces.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteDBSecurityGroup

instance AWSRequest DeleteDBSecurityGroup where
    type Er DeleteDBSecurityGroup = RDSError
    type Rs DeleteDBSecurityGroup = DeleteDBSecurityGroupResponse
    request = getQuery service "DeleteDBSecurityGroup"

data DeleteDBSecurityGroupResponse = DeleteDBSecurityGroupResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteDBSecurityGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot DeleteDBSecurityGroupResponse
