{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.GetRole
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves information about the specified role, including the role's path,
-- GUID, ARN, and the policy granting permission to EC2 to assume the role.
-- For more information about ARNs, go to ARNs. For more information about
-- roles, go to Working with Roles. The returned policy is URL-encoded
-- according to RFC 3986. For more information about RFC 3986, go to
-- http://www.faqs.org/rfcs/rfc3986.html. https://iam.amazonaws.com/
-- ?Action=GetRole &RoleName=S3Access &Version=2010-05-08 &AUTHPARAMS
-- /application_abc/component_xyz/
-- arn:aws:iam::123456789012:role/application_abc/component_xyz/S3Access
-- S3Access
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":["ec2.amazonaws.com"]},"Action":["sts:AssumeRole"]}]}
-- 2012-05-08T23:34:01Z AROADBQP57FF2AEXAMPLE
-- df37e965-9967-11e1-a4c3-270EXAMPLE04.
module Network.AWS.IAM.GetRole where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
getRole :: Text
        -> GetRole
getRole p1 = GetRole
    { grrRoleName = p1
    }

data GetRole = GetRole
    { grrRoleName :: !Text
      -- ^ Name of the role to get information about.
    } deriving (Eq, Show, Generic)

instance ToQuery GetRole

instance AWSRequest GetRole where
    type Er GetRole = IAMError
    type Rs GetRole = GetRoleResponse
    request = getQuery service "GetRole"

data GetRoleResponse = GetRoleResponse
    { grrrsRole :: Role
      -- ^ Information about the role.
    } deriving (Eq, Show, Generic)

instance FromXML GetRoleResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetRoleResponse"
        :| ["GetRoleResult"]
