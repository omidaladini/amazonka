{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.CreateDBSecurityGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new DB security group. DB security groups control access to a DB
-- instance. https://rds.amazonaws.com/ ?Action=CreateDBSecurityGroup
-- &DBSecurityGroupName=mydbsecuritygroup
-- &DBSecurityGroupDescription=My%20new%20DBSecurityGroup
-- &EC2VpcId=vpc-1a2b3c4d &Version=2013-05-15 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-02-15T18%3A14%3A49.482Z
-- &AWSAccessKeyId= &Signature= My new DBSecurityGroup 565419523791
-- mydbsecuritygroup vpc-1a2b3c4d ed662948-a57b-11df-9e38-7ffab86c801f.
module Network.AWS.RDS.CreateDBSecurityGroup where

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
createDBSecurityGroup :: Text
                      -> Text
                      -> CreateDBSecurityGroup
createDBSecurityGroup p1 p2 = CreateDBSecurityGroup
    { cdbsgmDBSecurityGroupDescription = p1
    , cdbsgmDBSecurityGroupName = p2
    , cdbsgmTags = []
    }

data CreateDBSecurityGroup = CreateDBSecurityGroup
    { cdbsgmDBSecurityGroupDescription :: !Text
      -- ^ The description for the DB security group.
    , cdbsgmDBSecurityGroupName :: !Text
      -- ^ The name for the DB security group. This value is stored as a lowercase
      -- string. Constraints: Must be 1 to 255 alphanumeric characters First
      -- character must be a letter Cannot end with a hyphen or contain two
      -- consecutive hyphens Must not be "Default" May not contain spaces Example:
      -- mysecuritygroup.
    , cdbsgmTags :: [Tag]
      -- ^ A list of tags.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateDBSecurityGroup

instance AWSRequest CreateDBSecurityGroup where
    type Er CreateDBSecurityGroup = RDSError
    type Rs CreateDBSecurityGroup = CreateDBSecurityGroupResponse
    request = getQuery service "CreateDBSecurityGroup"

data CreateDBSecurityGroupResponse = CreateDBSecurityGroupResponse
    { cdbsgmrsDBSecurityGroup :: Maybe DBSecurityGroup
      -- ^ Contains the result of a successful invocation of the following actions:
      -- DescribeDBSecurityGroups AuthorizeDBSecurityGroupIngress
      -- CreateDBSecurityGroup RevokeDBSecurityGroupIngress This data type is used
      -- as a response element in the DescribeDBSecurityGroups action.
    } deriving (Eq, Show, Generic)

instance FromXML CreateDBSecurityGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateDBSecurityGroupResponse"
        :| ["CreateDBSecurityGroupResult"]
