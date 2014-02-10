{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.DeleteAccessKey
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the access key associated with the specified user. If you do not
-- specify a user name, IAM determines the user name implicitly based on the
-- AWS access key ID signing the request. Because this action works for access
-- keys under the AWS account, you can use this API to manage root credentials
-- even if the AWS account has no associated users. https://iam.amazonaws.com/
-- ?Action=DeleteAccessKey &UserName=Bob &AccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &Version=2010-05-08 &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.DeleteAccessKey where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteAccessKey :: Text
                -- ^ The access key ID for the access key ID and secret access key you want to
                -- delete.
                -> DeleteAccessKey
deleteAccessKey p1 = DeleteAccessKey
    { dakAccessKeyId = p1
    , dakUserName = Nothing
    }

data DeleteAccessKey = DeleteAccessKey
    { dakAccessKeyId :: !Text
      -- ^ The access key ID for the access key ID and secret access key you want to
      -- delete.
    , dakUserName :: Maybe Text
      -- ^ Name of the user whose key you want to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteAccessKey

instance AWSRequest DeleteAccessKey where
    type Er DeleteAccessKey = IAMError
    type Rs DeleteAccessKey = DeleteAccessKeyResponse
    request  = postQuery service "DeleteAccessKey"
    response = responseXML

data DeleteAccessKeyResponse = DeleteAccessKeyResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteAccessKeyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteAccessKeyResponse"
