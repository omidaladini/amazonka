{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.RemoveRoleFromInstanceProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes the specified role from the specified instance profile. Make sure
-- you do not have any Amazon EC2 instances running with the role you are
-- about to remove from the instance profile. Removing a role from an instance
-- profile that is associated with a running instance will break any
-- applications running on the instance. For more information about roles, go
-- to Working with Roles. For more information about instance profiles, go to
-- About Instance Profiles. https://iam.amazonaws.com/
-- ?Action=RemoveRoleFromInstanceProfile &InstanceProfileName=Webserver
-- &RoleName=S3Access &Version=2010-05-08 &AUTHPARAMS
-- 29f47818-99f5-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.RemoveRoleFromInstanceProfile where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
removeRoleFromInstanceProfile :: Text
                              -> Text
                              -> RemoveRoleFromInstanceProfile
removeRoleFromInstanceProfile p1 p2 = RemoveRoleFromInstanceProfile
    { rrfiprInstanceProfileName = p1
    , rrfiprRoleName = p2
    }

data RemoveRoleFromInstanceProfile = RemoveRoleFromInstanceProfile
    { rrfiprInstanceProfileName :: !Text
      -- ^ Name of the instance profile to update.
    , rrfiprRoleName :: !Text
      -- ^ Name of the role to remove.
    } deriving (Eq, Show, Generic)

instance ToQuery RemoveRoleFromInstanceProfile

instance AWSRequest RemoveRoleFromInstanceProfile where
    type Er RemoveRoleFromInstanceProfile = IAMError
    type Rs RemoveRoleFromInstanceProfile = RemoveRoleFromInstanceProfileResponse
    request = getQuery service "RemoveRoleFromInstanceProfile"

data RemoveRoleFromInstanceProfileResponse = RemoveRoleFromInstanceProfileResponse
    deriving (Eq, Show, Generic)

instance FromXML RemoveRoleFromInstanceProfileResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RemoveRoleFromInstanceProfileResponse"
