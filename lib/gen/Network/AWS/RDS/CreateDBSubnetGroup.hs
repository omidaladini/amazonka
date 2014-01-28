{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.CreateDBSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new DB subnet group. DB subnet groups must contain at least one
-- subnet in at least two AZs in the region. https://rds.amazonaws.com/
-- ?Action=CreateDBSubnetGroup &DBSubnetGroupName=mydbsubnetgroup
-- &DBSubnetGroupDescription=My%20new%20DBSubnetGroup &Version=2013-05-15
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-02-15T18%3A14%3A49.482Z &AWSAccessKeyId= &Signature=
-- 990524496922 Complete My new DBSubnetGroup mydbsubnetgroup Active
-- subnet-7c5b4115 us-east-1c Active subnet-7b5b4112 us-east-1b Active
-- subnet-3ea6bd57 us-east-1d ed662948-a57b-11df-9e38-7ffab86c801f.
module Network.AWS.RDS.CreateDBSubnetGroup where

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

data CreateDBSubnetGroup = CreateDBSubnetGroup
    { cdbsgnDBSubnetGroupDescription :: !Text
      -- ^ The description for the DB subnet group.
    , cdbsgnDBSubnetGroupName :: !Text
      -- ^ The name for the DB subnet group. This value is stored as a lowercase
      -- string. Constraints: Must contain no more than 255 alphanumeric characters
      -- or hyphens. Must not be "Default". Example: mySubnetgroup.
    , cdbsgnSubnetIds :: [Text]
      -- ^ The EC2 Subnet IDs for the DB subnet group.
    , cdbsgnTags :: [Tag]
      -- ^ A list of tags.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateDBSubnetGroup

instance AWSRequest CreateDBSubnetGroup where
    type Er CreateDBSubnetGroup = RDSError
    type Rs CreateDBSubnetGroup = CreateDBSubnetGroupResponse
    request = getQuery service "CreateDBSubnetGroup"

data CreateDBSubnetGroupResponse = CreateDBSubnetGroupResponse
    { cdbsgnrsDBSubnetGroup :: Maybe DBSubnetGroup
      -- ^ Contains the result of a successful invocation of the following actions:
      -- CreateDBSubnetGroup ModifyDBSubnetGroup DescribeDBSubnetGroups
      -- DeleteDBSubnetGroup This data type is used as a response element in the
      -- DescribeDBSubnetGroups action.
    } deriving (Eq, Show, Generic)

instance FromXML CreateDBSubnetGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateDBSubnetGroupResponse"
        :| ["CreateDBSubnetGroupResult"]
