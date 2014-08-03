{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.V2010_05_08.CreateLoginProfile
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
-- information about managing passwords, see Managing Passwords in the Using
-- IAM guide. https://iam.amazonaws.com/ ?Action=CreateLoginProfile
-- &UserName=Bob &Password=Password1 &AUTHPARAMS Bob 2011-09-19T23:00:56Z
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.CreateLoginProfile where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateLoginProfile' request.
createLoginProfile :: Text -- ^ '_clprPassword'
                   -> Text -- ^ '_clprUserName'
                   -> CreateLoginProfile
createLoginProfile p1 p2 = CreateLoginProfile
    { _clprPassword = p1
    , _clprUserName = p2
    , _clprPasswordResetRequired = Nothing
    }

data CreateLoginProfile = CreateLoginProfile
    { _clprPassword :: Text
      -- ^ The new password for the user.
    , _clprUserName :: Text
      -- ^ Name of the user to create a password for.
    , _clprPasswordResetRequired :: Maybe Bool
      -- ^ Specifies whether the user is required to set a new password on
      -- next sign-in.
    } deriving (Generic)

makeLenses ''CreateLoginProfile

instance ToQuery CreateLoginProfile where
    toQuery = genericToQuery def

data CreateLoginProfileResponse = CreateLoginProfileResponse
    { _clpsLoginProfile :: LoginProfile
      -- ^ The user name and password create date.
    } deriving (Generic)

makeLenses ''CreateLoginProfileResponse

instance FromXML CreateLoginProfileResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateLoginProfile where
    type Sv CreateLoginProfile = IAM
    type Rs CreateLoginProfile = CreateLoginProfileResponse

    request = post "CreateLoginProfile"
    response _ = xmlResponse