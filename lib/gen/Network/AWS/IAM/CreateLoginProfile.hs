{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.CreateLoginProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a password for the specified user, giving the user the ability to
-- access AWS services through the AWS Management Console. For more
-- information about managing passwords, see Managing Passwords in Using IAM.
-- https://iam.amazonaws.com/ ?Action=CreateLoginProfile &UserName=Bob
-- &Password=Password1 &AUTHPARAMS Bob 2011-09-19T23:00:56Z
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.CreateLoginProfile where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createLoginProfile :: Text
                   -- ^ The new password for the user name.
                   -> Text
                   -- ^ Name of the user to create a password for.
                   -> CreateLoginProfile
createLoginProfile p1 p2 = CreateLoginProfile
    { clprPassword = p1
    , clprUserName = p2
    }

data CreateLoginProfile = CreateLoginProfile
    { clprPassword :: !Text
      -- ^ The new password for the user name.
    , clprUserName :: !Text
      -- ^ Name of the user to create a password for.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateLoginProfile

instance AWSRequest CreateLoginProfile where
    type Er CreateLoginProfile = IAMError
    type Rs CreateLoginProfile = CreateLoginProfileResponse
    request = getQuery service "CreateLoginProfile"

data CreateLoginProfileResponse = CreateLoginProfileResponse
    { clprrsLoginProfile :: LoginProfile
      -- ^ The user name and password create date.
    } deriving (Eq, Show, Generic)

instance FromXML CreateLoginProfileResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateLoginProfileResponse"
        :| ["CreateLoginProfileResult"]
