{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.PutGroupPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds (or updates) a policy document associated with the specified group.
-- For information about policies, refer to Overview of Policies in Using AWS
-- Identity and Access Management. For information about limits on the number
-- of policies you can associate with a group, see Limitations on IAM Entities
-- in Using AWS Identity and Access Management. Because policy documents can
-- be large, you should use POST rather than GET when calling PutGroupPolicy.
-- For information about setting up signatures and authorization through the
-- API, go to Signing AWS API Requests in the AWS General Reference. For
-- general information about using the Query API with IAM, go to Making Query
-- Requests in Using IAM. https://iam.amazonaws.com/ ?Action=PutGroupPolicy
-- &GroupName=Admins &PolicyName=AdminRoot
-- &PolicyDocument={"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":"*","Resource":"*"}]}
-- &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.PutGroupPolicy where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
putGroupPolicy :: Text
               -- ^ Name of the group to associate the policy with.
               -> Text
               -- ^ The policy document.
               -> Text
               -- ^ Name of the policy document.
               -> PutGroupPolicy
putGroupPolicy p1 p2 p3 = PutGroupPolicy
    { pgprGroupName = p1
    , pgprPolicyDocument = p2
    , pgprPolicyName = p3
    }

data PutGroupPolicy = PutGroupPolicy
    { pgprGroupName :: !Text
      -- ^ Name of the group to associate the policy with.
    , pgprPolicyDocument :: !Text
      -- ^ The policy document.
    , pgprPolicyName :: !Text
      -- ^ Name of the policy document.
    } deriving (Eq, Show, Generic)

instance ToQuery PutGroupPolicy

instance AWSRequest PutGroupPolicy where
    type Er PutGroupPolicy = IAMError
    type Rs PutGroupPolicy = PutGroupPolicyResponse
    request = getQuery service "PutGroupPolicy"

data PutGroupPolicyResponse = PutGroupPolicyResponse
    deriving (Eq, Show, Generic)

instance FromXML PutGroupPolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PutGroupPolicyResponse"
