{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.DeleteRole
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified role. The role must not have any policies attached.
-- For more information about roles, go to Working with Roles. Make sure you
-- do not have any Amazon EC2 instances running with the role you are about to
-- delete. Deleting a role or instance profile that is associated with a
-- running instance will break any applications running on the instance.
-- https://iam.amazonaws.com/ ?Action=DeleteRole &RoleName=S3Access
-- &Version=2010-05-08 &AUTHPARAMS 913e3f37-99ed-11e1-a4c3-270EXAMPLE04.
module Network.AWS.IAM.DeleteRole where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteRole :: Text
           -- ^ Name of the role to delete.
           -> DeleteRole
deleteRole p1 = DeleteRole
    { drrRoleName = p1
    }

data DeleteRole = DeleteRole
    { drrRoleName :: !Text
      -- ^ Name of the role to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteRole

instance AWSRequest DeleteRole where
    type Er DeleteRole = IAMError
    type Rs DeleteRole = DeleteRoleResponse
    request = getQuery service "DeleteRole"

data DeleteRoleResponse = DeleteRoleResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteRoleResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteRoleResponse"
