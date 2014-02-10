{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.AddUserToGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds the specified user to the specified group. https://iam.amazonaws.com/
-- ?Action=AddUserToGroup &GroupName=Managers &UserName=Bob &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.AddUserToGroup where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

data AddUserToGroup = AddUserToGroup
    { autgGroupName :: !Text
      -- ^ Name of the group to update.
    , autgUserName :: !Text
      -- ^ Name of the user to add.
    } deriving (Eq, Show, Generic)

instance ToQuery AddUserToGroup

instance AWSRequest AddUserToGroup where
    type Er AddUserToGroup = IAMError
    type Rs AddUserToGroup = AddUserToGroupResponse
    request  = postQuery service "AddUserToGroup"
    response = responseXML

data AddUserToGroupResponse = AddUserToGroupResponse
    deriving (Eq, Show, Generic)

instance FromXML AddUserToGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AddUserToGroupResponse"
