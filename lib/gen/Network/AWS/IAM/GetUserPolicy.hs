{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.GetUserPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves the specified policy document for the specified user. The
-- returned policy is URL-encoded according to RFC 3986. For more information
-- about RFC 3986, go to http://www.faqs.org/rfcs/rfc3986.html.
-- https://iam.amazonaws.com/ ?Action=GetUserPolicy &UserName=Bob
-- &PolicyName=AllAccessPolicy &AUTHPARAMS Bob AllAccessPolicy
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":"*","Resource":"*"}]}
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.GetUserPolicy where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
getUserPolicy :: Text
              -> Text
              -> GetUserPolicy
getUserPolicy p1 p2 = GetUserPolicy
    { guprPolicyName = p1
    , guprUserName = p2
    }

data GetUserPolicy = GetUserPolicy
    { guprPolicyName :: !Text
      -- ^ Name of the policy document to get.
    , guprUserName :: !Text
      -- ^ Name of the user who the policy is associated with.
    } deriving (Eq, Show, Generic)

instance ToQuery GetUserPolicy

instance AWSRequest GetUserPolicy where
    type Er GetUserPolicy = IAMError
    type Rs GetUserPolicy = GetUserPolicyResponse
    request = getQuery service "GetUserPolicy"

data GetUserPolicyResponse = GetUserPolicyResponse
    { guprrsPolicyDocument :: !Text
      -- ^ The policy document.
    , guprrsPolicyName :: !Text
      -- ^ The name of the policy.
    , guprrsUserName :: !Text
      -- ^ The user the policy is associated with.
    } deriving (Eq, Show, Generic)

instance FromXML GetUserPolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetUserPolicyResponse"
        :| ["GetUserPolicyResult"]
