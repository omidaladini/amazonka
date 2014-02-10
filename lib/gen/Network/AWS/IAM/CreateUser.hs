{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.CreateUser
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new user for your AWS account. For information about limitations
-- on the number of users you can create, see Limitations on IAM Entities in
-- Using AWS Identity and Access Management. https://iam.amazonaws.com/
-- ?Action=CreateUser &Path=/division_abc/subdivision_xyz/ &UserName=Bob
-- &Version=2010-05-08 &AUTHPARAMS /division_abc/subdivision_xyz/ Bob
-- AIDACKCEVSQ6C2EXAMPLE
-- arn:aws:iam::123456789012:user/division_abc/subdivision_xyz/Bob
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.CreateUser where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createUser :: Text
           -- ^ Name of the user to create.
           -> CreateUser
createUser p1 = CreateUser
    { cuUserName = p1
    , cuPath = Nothing
    }

data CreateUser = CreateUser
    { cuPath :: Maybe Text
      -- ^ The path for the user name. For more information about paths, see
      -- Identifiers for IAM Entities in Using AWS Identity and Access Management.
      -- This parameter is optional. If it is not included, it defaults to a slash
      -- (/).
    , cuUserName :: !Text
      -- ^ Name of the user to create.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateUser

instance AWSRequest CreateUser where
    type Er CreateUser = IAMError
    type Rs CreateUser = CreateUserResponse
    request  = postQuery service "CreateUser"
    response = responseXML

data CreateUserResponse = CreateUserResponse
    { curUser :: Maybe User
      -- ^ Information about the user.
    } deriving (Eq, Show, Generic)

instance FromXML CreateUserResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateUserResponse"
        :| ["CreateUserResult"]
