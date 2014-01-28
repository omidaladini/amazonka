{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.CreateGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new group. For information about the number of groups you can
-- create, see Limitations on IAM Entities in Using AWS Identity and Access
-- Management. https://iam.amazonaws.com/ ?Action=CreateGroup &Path=/
-- &GroupName=Admins &Version=2010-05-08 &AUTHPARAMS / Admins
-- AGPACKCEVSQ6C2EXAMPLE arn:aws:iam::123456789012:group/Admins
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.CreateGroup where

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

import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

data CreateGroup = CreateGroup
    { cgrGroupName :: !Text
      -- ^ Name of the group to create. Do not include the path in this value.
    , cgrPath :: Maybe Text
      -- ^ The path to the group. For more information about paths, see Identifiers
      -- for IAM Entities in Using AWS Identity and Access Management. This
      -- parameter is optional. If it is not included, it defaults to a slash (/).
    } deriving (Eq, Show, Generic)

instance ToQuery CreateGroup

instance AWSRequest CreateGroup where
    type Er CreateGroup = IAMError
    type Rs CreateGroup = CreateGroupResponse
    request = getQuery service "CreateGroup"

data CreateGroupResponse = CreateGroupResponse
    { cgrrsGroup :: Group
      -- ^ Information about the group.
    } deriving (Eq, Show, Generic)

instance FromXML CreateGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateGroupResponse"
        :| ["CreateGroupResult"]