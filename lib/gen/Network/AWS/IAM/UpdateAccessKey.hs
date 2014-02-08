{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.UpdateAccessKey
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Changes the status of the specified access key from Active to Inactive, or
-- vice versa. This action can be used to disable a user's key as part of a
-- key rotation work flow. If the UserName field is not specified, the
-- UserName is determined implicitly based on the AWS access key ID used to
-- sign the request. Because this action works for access keys under the AWS
-- account, this API can be used to manage root credentials even if the AWS
-- account has no associated users. For information about rotating keys, see
-- Managing Keys and Certificates in Using AWS Identity and Access Management.
-- https://iam.amazonaws.com/ ?Action=UpdateAccessKey &UserName=Bob
-- &AccessKeyId=AKIAIOSFODNN7EXAMPLE &Status=Inactive &Version=2010-05-08
-- &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.UpdateAccessKey where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
updateAccessKey :: Text
                -> StatusType
                -> UpdateAccessKey
updateAccessKey p1 p2 = UpdateAccessKey
    { uakrAccessKeyId = p1
    , uakrStatus = p2
    , uakrUserName = Nothing
    }

data UpdateAccessKey = UpdateAccessKey
    { uakrAccessKeyId :: !Text
      -- ^ The access key ID of the secret access key you want to update.
    , uakrStatus :: !StatusType
      -- ^ The status you want to assign to the secret access key. Active means the
      -- key can be used for API calls to AWS, while Inactive means the key cannot
      -- be used.
    , uakrUserName :: Maybe Text
      -- ^ Name of the user whose key you want to update.
    } deriving (Eq, Show, Generic)

instance ToQuery UpdateAccessKey

instance AWSRequest UpdateAccessKey where
    type Er UpdateAccessKey = IAMError
    type Rs UpdateAccessKey = UpdateAccessKeyResponse
    request = getQuery service "UpdateAccessKey"

data UpdateAccessKeyResponse = UpdateAccessKeyResponse
    deriving (Eq, Show, Generic)

instance FromXML UpdateAccessKeyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "UpdateAccessKeyResponse"
