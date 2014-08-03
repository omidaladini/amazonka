{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.RDS.V2013_09_09.CreateDBSubnetGroup
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
module Network.AWS.RDS.V2013_09_09.CreateDBSubnetGroup where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateDBSubnetGroup' request.
createDBSubnetGroup :: Text -- ^ '_cdbsgnDBSubnetGroupName'
                    -> Text -- ^ '_cdbsgnDBSubnetGroupDescription'
                    -> [Text] -- ^ '_cdbsgnSubnetIds'
                    -> CreateDBSubnetGroup
createDBSubnetGroup p1 p2 p3 = CreateDBSubnetGroup
    { _cdbsgnDBSubnetGroupName = p1
    , _cdbsgnDBSubnetGroupDescription = p2
    , _cdbsgnSubnetIds = p3
    , _cdbsgnTags = mempty
    }

data CreateDBSubnetGroup = CreateDBSubnetGroup
    { _cdbsgnDBSubnetGroupName :: Text
      -- ^ The name for the DB subnet group. This value is stored as a
      -- lowercase string. Constraints: Must contain no more than 255
      -- alphanumeric characters or hyphens. Must not be "Default".
      -- Example: mySubnetgroup.
    , _cdbsgnDBSubnetGroupDescription :: Text
      -- ^ The description for the DB subnet group.
    , _cdbsgnSubnetIds :: [Text]
      -- ^ The EC2 Subnet IDs for the DB subnet group.
    , _cdbsgnTags :: [Tag]
      -- ^ A list of tags.
    } deriving (Generic)

makeLenses ''CreateDBSubnetGroup

instance ToQuery CreateDBSubnetGroup where
    toQuery = genericToQuery def

data CreateDBSubnetGroupResponse = CreateDBSubnetGroupResponse
    { _dbsgdrDBSubnetGroup :: Maybe DBSubnetGroup
      -- ^ Contains the result of a successful invocation of the following
      -- actions: CreateDBSubnetGroup ModifyDBSubnetGroup
      -- DescribeDBSubnetGroups DeleteDBSubnetGroup This data type is used
      -- as a response element in the DescribeDBSubnetGroups action.
    } deriving (Generic)

makeLenses ''CreateDBSubnetGroupResponse

instance FromXML CreateDBSubnetGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateDBSubnetGroup where
    type Sv CreateDBSubnetGroup = RDS
    type Rs CreateDBSubnetGroup = CreateDBSubnetGroupResponse

    request = post "CreateDBSubnetGroup"
    response _ = xmlResponse