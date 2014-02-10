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

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

data RemoveUserFromGroup = RemoveUserFromGroup
    { rufgGroupName :: !Text
      -- ^ Name of the group to update.
    , rufgUserName :: !Text
      -- ^ Name of the user to remove.
    } deriving (Eq, Show, Generic)

instance ToQuery RemoveUserFromGroup

instance AWSRequest RemoveUserFromGroup where
    type Er RemoveUserFromGroup = IAMError
    type Rs RemoveUserFromGroup = RemoveUserFromGroupResponse
    request  = postQuery service "RemoveUserFromGroup"
    response = responseXML

data RemoveUserFromGroupResponse = RemoveUserFromGroupResponse
    deriving (Eq, Show, Generic)

instance FromXML RemoveUserFromGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RemoveUserFromGroupResponse"