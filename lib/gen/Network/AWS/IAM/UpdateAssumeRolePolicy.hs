{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.UpdateAssumeRolePolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the policy that grants an entity permission to assume a role.
-- Currently, only an Amazon EC2 instance can assume a role. For more
-- information about roles, go to Working with Roles.
-- https://iam.amazonaws.com/ ?Action=UpdateAssumeRolePolicy
-- &PolicyDocument={"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":["ec2.amazonaws.com"]},"Action":["sts:AssumeRole"]}]}
-- &RoleName=S3Access &Version=2010-05-08 &AUTHPARAMS
-- 309c1671-99ed-11e1-a4c3-270EXAMPLE04.
module Network.AWS.IAM.UpdateAssumeRolePolicy where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

data UpdateAssumeRolePolicy = UpdateAssumeRolePolicy
    { uarpPolicyDocument :: !Text
      -- ^ The policy that grants an entity permission to assume the role.
    , uarpRoleName :: !Text
      -- ^ Name of the role to update.
    } deriving (Eq, Show, Generic)

instance ToQuery UpdateAssumeRolePolicy

instance AWSRequest UpdateAssumeRolePolicy where
    type Er UpdateAssumeRolePolicy = IAMError
    type Rs UpdateAssumeRolePolicy = UpdateAssumeRolePolicyResponse
    request  = postQuery service "UpdateAssumeRolePolicy"
    response = responseXML

data UpdateAssumeRolePolicyResponse = UpdateAssumeRolePolicyResponse
    deriving (Eq, Show, Generic)

instance FromXML UpdateAssumeRolePolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "UpdateAssumeRolePolicyResponse"