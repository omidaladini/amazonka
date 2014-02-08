{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.DeleteRolePolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified policy associated with the specified role.
-- https://iam.amazonaws.com/ ?Action=DeleteRolePolicy
-- &PolicyName=S3AccessPolicy &RoleName=S3Access &Version=2010-05-08
-- &AUTHPARAMS c749ee7f-99ef-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.DeleteRolePolicy where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteRolePolicy :: Text
                 -- ^ Name of the policy document to delete.
                 -> Text
                 -- ^ Name of the role the associated with the policy.
                 -> DeleteRolePolicy
deleteRolePolicy p1 p2 = DeleteRolePolicy
    { drprPolicyName = p1
    , drprRoleName = p2
    }

data DeleteRolePolicy = DeleteRolePolicy
    { drprPolicyName :: !Text
      -- ^ Name of the policy document to delete.
    , drprRoleName :: !Text
      -- ^ Name of the role the associated with the policy.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteRolePolicy

instance AWSRequest DeleteRolePolicy where
    type Er DeleteRolePolicy = IAMError
    type Rs DeleteRolePolicy = DeleteRolePolicyResponse
    request = getQuery service "DeleteRolePolicy"

data DeleteRolePolicyResponse = DeleteRolePolicyResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteRolePolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteRolePolicyResponse"
