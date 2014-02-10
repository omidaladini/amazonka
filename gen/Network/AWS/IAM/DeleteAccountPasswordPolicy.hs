{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.DeleteAccountPasswordPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the password policy for the AWS account. https://iam.amazonaws.com/
-- ?Action=DeleteAccountPasswordPolicy &Version=2010-05-08 &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.DeleteAccountPasswordPolicy where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

data DeleteAccountPasswordPolicy = DeleteAccountPasswordPolicy
    deriving (Eq, Show, Generic)

instance ToQuery DeleteAccountPasswordPolicy

instance AWSRequest DeleteAccountPasswordPolicy where
    type Er DeleteAccountPasswordPolicy = IAMError
    type Rs DeleteAccountPasswordPolicy = DeleteAccountPasswordPolicyResponse
    request  = postQuery service "DeleteAccountPasswordPolicy"
    response = responseXML

data DeleteAccountPasswordPolicyResponse = DeleteAccountPasswordPolicyResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteAccountPasswordPolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteAccountPasswordPolicyResponse"
