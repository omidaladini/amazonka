{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateSecurityGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateSecurityGroup operation creates a new security group. Every
-- instance is launched in a security group. If no security group is specified
-- during launch, the instances are launched in the default security group.
-- Instances within the same security group have unrestricted network access
-- to each other. Instances will reject network access attempts from other
-- instances in a different security group. As the owner of instances you can
-- grant or revoke specific permissions using the
-- AuthorizeSecurityGroupIngress and RevokeSecurityGroupIngress operations.
module Network.AWS.EC2.CreateSecurityGroup where

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
createSecurityGroup :: Text
                    -> Text
                    -> AWS (Either EC2Error CreateSecurityGroupResponse)
createSecurityGroup p1 p2 = undefined $ CreateSecurityGroup
    { csgrDescription = p1
    , csgrGroupName = p2
    , csgrDryRun = Nothing
    , csgrVpcId = Nothing
    }

data CreateSecurityGroup = CreateSecurityGroup
    { csgrDescription :: !Text
      -- ^ Description of the group. This is informational only.
    , csgrDryRun :: Maybe Bool
    , csgrGroupName :: !Text
      -- ^ Name of the security group.
    , csgrVpcId :: Maybe Text
      -- ^ ID of the VPC.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateSecurityGroup

instance AWSRequest CreateSecurityGroup where
    type Er CreateSecurityGroup = EC2Error
    type Rs CreateSecurityGroup = CreateSecurityGroupResponse
    request = getQuery service "CreateSecurityGroup"

data CreateSecurityGroupResponse = CreateSecurityGroupResponse
    { csgrrsGroupId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromXML CreateSecurityGroupResponse where
    fromXMLOptions = xmlOptions
