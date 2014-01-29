{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.CreateDBParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new DB parameter group. A DB parameter group is initially created
-- with the default parameters for the database engine used by the DB
-- instance. To provide custom values for any of the parameters, you must
-- modify the group after creating it using ModifyDBParameterGroup. Once
-- you've created a DB parameter group, you need to associate it with your DB
-- instance using ModifyDBInstance. When you associate a new DB parameter
-- group with a running DB instance, you need to reboot the DB Instance for
-- the new DB parameter group and associated settings to take effect.
-- https://rds.amazonaws.com/ ?Action=CreateDBParameterGroup
-- &DBParameterGroupName=mydbparametergroup3 &DBParameterGroupFamily=MySQL5.1
-- &Version=2013-05-15 &Description=My%20new%20DBParameterGroup
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-11T18%3A09%3A29.793Z &AWSAccessKeyId= &Signature=
-- mysql5.1 My new DBParameterGroup mydbparametergroup3
-- 0b447b66-bf36-11de-a88b-7b5b3d23b3a7.
module Network.AWS.RDS.CreateDBParameterGroup where

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
createDBParameterGroup :: Text
                       -> Text
                       -> Text
                       -> CreateDBParameterGroup
createDBParameterGroup p1 p2 p3 = undefined $ CreateDBParameterGroup
    { cdbpgmDBParameterGroupFamily = p1
    , cdbpgmDBParameterGroupName = p2
    , cdbpgmDescription = p3
    , cdbpgmTags = []
    }

data CreateDBParameterGroup = CreateDBParameterGroup
    { cdbpgmDBParameterGroupFamily :: !Text
      -- ^ The DB parameter group family name. A DB parameter group can be associated
      -- with one and only one DB parameter group family, and can be applied only to
      -- a DB instance running a database engine and engine version compatible with
      -- that DB parameter group family.
    , cdbpgmDBParameterGroupName :: !Text
      -- ^ The name of the DB parameter group. Constraints: Must be 1 to 255
      -- alphanumeric characters First character must be a letter Cannot end with a
      -- hyphen or contain two consecutive hyphens This value is stored as a
      -- lower-case string.
    , cdbpgmDescription :: !Text
      -- ^ The description for the DB parameter group.
    , cdbpgmTags :: [Tag]
      -- ^ A list of tags.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateDBParameterGroup

instance AWSRequest CreateDBParameterGroup where
    type Er CreateDBParameterGroup = RDSError
    type Rs CreateDBParameterGroup = CreateDBParameterGroupResponse
    request = getQuery service "CreateDBParameterGroup"

data CreateDBParameterGroupResponse = CreateDBParameterGroupResponse
    { cdbpgmrsDBParameterGroup :: Maybe DBParameterGroup
      -- ^ Contains the result of a successful invocation of the
      -- CreateDBParameterGroup action. This data type is used as a request
      -- parameter in the DeleteDBParameterGroup action, and as a response element
      -- in the DescribeDBParameterGroups action.
    } deriving (Eq, Show, Generic)

instance FromXML CreateDBParameterGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateDBParameterGroupResponse"
        :| ["CreateDBParameterGroupResult"]
