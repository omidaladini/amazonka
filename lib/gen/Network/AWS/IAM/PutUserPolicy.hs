{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.PutUserPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds (or updates) a policy document associated with the specified user. For
-- information about policies, refer to Overview of Policies in Using AWS
-- Identity and Access Management. For information about limits on the number
-- of policies you can associate with a user, see Limitations on IAM Entities
-- in Using AWS Identity and Access Management. Because policy documents can
-- be large, you should use POST rather than GET when calling PutUserPolicy.
-- For information about setting up signatures and authorization through the
-- API, go to Signing AWS API Requests in the AWS General Reference. For
-- general information about using the Query API with IAM, go to Making Query
-- Requests in Using IAM. https://iam.amazonaws.com/ ?Action=PutUserPolicy
-- &UserName=Bob &PolicyName=AllAccessPolicy
-- &PolicyDocument={"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":"*","Resource":"*"}]}
-- &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.PutUserPolicy where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

data PutUserPolicy = PutUserPolicy
    { pupPolicyDocument :: !Text
      -- ^ The policy document.
    , pupPolicyName :: !Text
      -- ^ Name of the policy document.
    , pupUserName :: !Text
      -- ^ Name of the user to associate the policy with.
    } deriving (Eq, Show, Generic)

instance ToQuery PutUserPolicy

instance AWSRequest PutUserPolicy where
    type Er PutUserPolicy = IAMError
    type Rs PutUserPolicy = PutUserPolicyResponse
    request  = postQuery service "PutUserPolicy"
    response = responseXML

data PutUserPolicyResponse = PutUserPolicyResponse
    deriving (Eq, Show, Generic)

instance FromXML PutUserPolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PutUserPolicyResponse"
