{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.DeleteGroupPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified policy that is associated with the specified group.
-- https://iam.amazonaws.com/ ?Action=DeleteGroupPolicy &GroupName=Admins
-- &PolicyName=AdminRoot &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.DeleteGroupPolicy where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteGroupPolicy :: Text
                  -- ^ Name of the group the policy is associated with.
                  -> Text
                  -- ^ Name of the policy document to delete.
                  -> DeleteGroupPolicy
deleteGroupPolicy p1 p2 = DeleteGroupPolicy
    { dgprGroupName = p1
    , dgprPolicyName = p2
    }

data DeleteGroupPolicy = DeleteGroupPolicy
    { dgprGroupName :: !Text
      -- ^ Name of the group the policy is associated with.
    , dgprPolicyName :: !Text
      -- ^ Name of the policy document to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteGroupPolicy

instance AWSRequest DeleteGroupPolicy where
    type Er DeleteGroupPolicy = IAMError
    type Rs DeleteGroupPolicy = DeleteGroupPolicyResponse
    request = getQuery service "DeleteGroupPolicy"

data DeleteGroupPolicyResponse = DeleteGroupPolicyResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteGroupPolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteGroupPolicyResponse"
