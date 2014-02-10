{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.PutRolePolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds (or updates) a policy document associated with the specified role. For
-- information about policies, go to Overview of Policies in Using AWS
-- Identity and Access Management. For information about limits on the
-- policies you can associate with a role, see Limitations on IAM Entities in
-- Using AWS Identity and Access Management. Because policy documents can be
-- large, you should use POST rather than GET when calling PutRolePolicy. For
-- information about setting up signatures and authorization through the API,
-- go to Signing AWS API Requests in the AWS General Reference. For general
-- information about using the Query API with IAM, go to Making Query Requests
-- in Using IAM. https://iam.amazonaws.com/ ?Action=PutRolePolicy
-- &RoleName=S3Access &PolicyName=S3AccessPolicy
-- &PolicyDocument={"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":"s3:*","Resource":"*"}]}
-- &Version=2010-05-08 &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.PutRolePolicy where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

data PutRolePolicy = PutRolePolicy
    { prpPolicyDocument :: !Text
      -- ^ The policy document.
    , prpPolicyName :: !Text
      -- ^ Name of the policy document.
    , prpRoleName :: !Text
      -- ^ Name of the role to associate the policy with.
    } deriving (Eq, Show, Generic)

instance ToQuery PutRolePolicy

instance AWSRequest PutRolePolicy where
    type Er PutRolePolicy = IAMError
    type Rs PutRolePolicy = PutRolePolicyResponse
    request  = postQuery service "PutRolePolicy"
    response = responseXML

data PutRolePolicyResponse = PutRolePolicyResponse
    deriving (Eq, Show, Generic)

instance FromXML PutRolePolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PutRolePolicyResponse"