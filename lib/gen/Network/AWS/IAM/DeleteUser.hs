{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.DeleteUser
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified user. The user must not belong to any groups, have
-- any keys or signing certificates, or have any attached policies.
-- https://iam.amazonaws.com/ ?Action=DeleteUser &UserName=Bob
-- &Version=2010-05-08 &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.DeleteUser where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

data DeleteUser = DeleteUser
    { duUserName :: !Text
      -- ^ Name of the user to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteUser

instance AWSRequest DeleteUser where
    type Er DeleteUser = IAMError
    type Rs DeleteUser = DeleteUserResponse
    request  = postQuery service "DeleteUser"
    response = responseXML

data DeleteUserResponse = DeleteUserResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteUserResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteUserResponse"
