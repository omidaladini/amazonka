{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.GetLoginProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves the user name and password-creation date for the specified user.
-- If the user has not been assigned a password, the action returns a 404
-- (NoSuchEntity) error. https://iam.amazonaws.com/ ?Action=GetLoginProfile
-- &UserName=Bob &AUTHPARAMS Bob 2011-09-19T23:00:56Z
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.GetLoginProfile where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
getLoginProfile :: Text
                -- ^ Name of the user whose login profile you want to retrieve.
                -> GetLoginProfile
getLoginProfile p1 = GetLoginProfile
    { glprUserName = p1
    }

data GetLoginProfile = GetLoginProfile
    { glprUserName :: !Text
      -- ^ Name of the user whose login profile you want to retrieve.
    } deriving (Eq, Show, Generic)

instance ToQuery GetLoginProfile

instance AWSRequest GetLoginProfile where
    type Er GetLoginProfile = IAMError
    type Rs GetLoginProfile = GetLoginProfileResponse
    request = getQuery service "GetLoginProfile"

data GetLoginProfileResponse = GetLoginProfileResponse
    { glprrLoginProfile :: LoginProfile
      -- ^ User name and password create date for the user.
    } deriving (Eq, Show, Generic)

instance FromXML GetLoginProfileResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetLoginProfileResponse"
        :| ["GetLoginProfileResult"]
