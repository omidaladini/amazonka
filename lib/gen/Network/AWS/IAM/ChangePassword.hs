{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ChangePassword
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Changes the password of the IAM user calling ChangePassword. The root
-- account password is not affected by this action. For information about
-- modifying passwords, see Managing Passwords. https://iam.amazonaws.com/
-- ?Action=ChangePassword &OldPassword=U79}kgds4? &NewPassword=Lb0*1(9xpN
-- &Version=2010-05-08 &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.ChangePassword where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

data ChangePassword = ChangePassword
    { cpNewPassword :: !Text
    , cpOldPassword :: !Text
    } deriving (Eq, Show, Generic)

instance ToQuery ChangePassword

instance AWSRequest ChangePassword where
    type Er ChangePassword = IAMError
    type Rs ChangePassword = ChangePasswordResponse
    request  = postQuery service "ChangePassword"
    response = responseXML

data ChangePasswordResponse = ChangePasswordResponse
    deriving (Eq, Show, Generic)

instance FromXML ChangePasswordResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ChangePasswordResponse"