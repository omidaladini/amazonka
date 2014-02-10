{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.AddRoleToInstanceProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds the specified role to the specified instance profile. For more
-- information about roles, go to Working with Roles. For more information
-- about instance profiles, go to About Instance Profiles.
-- https://iam.amazonaws.com/ ?Action=AddRoleToInstanceProfile
-- &InstanceProfileName=Webserver &RoleName=S3Access &Version=2010-05-08
-- &AUTHPARAMS 12657608-99f2-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.AddRoleToInstanceProfile where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

data AddRoleToInstanceProfile = AddRoleToInstanceProfile
    { artipInstanceProfileName :: !Text
      -- ^ Name of the instance profile to update.
    , artipRoleName :: !Text
      -- ^ Name of the role to add.
    } deriving (Eq, Show, Generic)

instance ToQuery AddRoleToInstanceProfile

instance AWSRequest AddRoleToInstanceProfile where
    type Er AddRoleToInstanceProfile = IAMError
    type Rs AddRoleToInstanceProfile = AddRoleToInstanceProfileResponse
    request  = postQuery service "AddRoleToInstanceProfile"
    response = responseXML

data AddRoleToInstanceProfileResponse = AddRoleToInstanceProfileResponse
    deriving (Eq, Show, Generic)

instance FromXML AddRoleToInstanceProfileResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AddRoleToInstanceProfileResponse"