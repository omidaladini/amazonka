{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.DeleteUserPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified policy associated with the specified user.
-- https://iam.amazonaws.com/ ?Action=DeleteUserPolicy &UserName=Bob
-- &PolicyName=AllAccessPolicy &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.DeleteUserPolicy where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteUserPolicy :: Text
                 -- ^ Name of the policy document to delete.
                 -> Text
                 -- ^ Name of the user the policy is associated with.
                 -> DeleteUserPolicy
deleteUserPolicy p1 p2 = DeleteUserPolicy
    { duprPolicyName = p1
    , duprUserName = p2
    }

data DeleteUserPolicy = DeleteUserPolicy
    { duprPolicyName :: !Text
      -- ^ Name of the policy document to delete.
    , duprUserName :: !Text
      -- ^ Name of the user the policy is associated with.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteUserPolicy

instance AWSRequest DeleteUserPolicy where
    type Er DeleteUserPolicy = IAMError
    type Rs DeleteUserPolicy = DeleteUserPolicyResponse
    request = getQuery service "DeleteUserPolicy"

data DeleteUserPolicyResponse = DeleteUserPolicyResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteUserPolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteUserPolicyResponse"
