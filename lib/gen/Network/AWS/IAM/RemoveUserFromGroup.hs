{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.RemoveUserFromGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes the specified user from the specified group.
-- https://iam.amazonaws.com/ ?Action=RemoveUserFromGroup &GroupName=Managers
-- &UserName=Bob &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.RemoveUserFromGroup where

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

-- | Convenience method utilising default fields where applicable.
removeUserFromGroup :: Text
                    -> Text
                    -> AWS (Either IAMError RemoveUserFromGroupResponse)
removeUserFromGroup p1 p2 = undefined $ RemoveUserFromGroup
    { rufgrGroupName = p1
    , rufgrUserName = p2
    }

data RemoveUserFromGroup = RemoveUserFromGroup
    { rufgrGroupName :: !Text
      -- ^ Name of the group to update.
    , rufgrUserName :: !Text
      -- ^ Name of the user to remove.
    } deriving (Eq, Show, Generic)

instance ToQuery RemoveUserFromGroup

instance AWSRequest RemoveUserFromGroup where
    type Er RemoveUserFromGroup = IAMError
    type Rs RemoveUserFromGroup = RemoveUserFromGroupResponse
    request = getQuery service "RemoveUserFromGroup"

data RemoveUserFromGroupResponse = RemoveUserFromGroupResponse
    deriving (Eq, Show, Generic)

instance FromXML RemoveUserFromGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "RemoveUserFromGroupResponse"
        :| ["RemoveUserFromGroupResult"]
