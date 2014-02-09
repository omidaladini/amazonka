{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ListAccessKeys
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about the access key IDs associated with the specified
-- user. If there are none, the action returns an empty list. Although each
-- user is limited to a small number of keys, you can still paginate the
-- results using the MaxItems and Marker parameters. If the UserName field is
-- not specified, the UserName is determined implicitly based on the AWS
-- access key ID used to sign the request. Because this action works for
-- access keys under the AWS account, this API can be used to manage root
-- credentials even if the AWS account has no associated users. To ensure the
-- security of your AWS account, the secret access key is accessible only
-- during key and user creation. https://iam.amazonaws.com/
-- ?Action=ListAccessKeys &UserName=Bob &Version=2010-05-08 &AUTHPARAMS Bob
-- Bob AKIAIOSFODNN7EXAMPLE Active Bob AKIAI44QH8DHBEXAMPLE Inactive false
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.ListAccessKeys where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

data ListAccessKeys = ListAccessKeys
    { lakrMarker :: Maybe Text
      -- ^ Use this parameter only when paginating results, and only in a subsequent
      -- request after you've received a response where the results are truncated.
      -- Set it to the value of the Marker element in the response you just
      -- received.
    , lakrMaxItems :: Maybe Int
      -- ^ Use this parameter only when paginating results to indicate the maximum
      -- number of keys you want in the response. If there are additional keys
      -- beyond the maximum you specify, the IsTruncated response element is true.
      -- This parameter is optional. If you do not include it, it defaults to 100.
    , lakrUserName :: Maybe Text
      -- ^ Name of the user.
    } deriving (Eq, Show, Generic)

instance ToQuery ListAccessKeys

instance AWSRequest ListAccessKeys where
    type Er ListAccessKeys = IAMError
    type Rs ListAccessKeys = ListAccessKeysResponse
    request = getQuery service "ListAccessKeys"

instance AWSPager ListAccessKeys where
    next rq rs
        | Just x <- lakrrMarker rs = Just $ rq { lakrMarker = Just x }
        | otherwise = Nothing

data ListAccessKeysResponse = ListAccessKeysResponse
    { lakrrAccessKeyMetadata :: [AccessKeyMetadata]
      -- ^ A list of access key metadata.
    , lakrrIsTruncated :: Maybe Bool
      -- ^ A flag that indicates whether there are more keys to list. If your results
      -- were truncated, you can make a subsequent pagination request using the
      -- Marker request parameter to retrieve more keys in the list.
    , lakrrMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the value to
      -- use for the Marker parameter in a subsequent pagination request.
    } deriving (Eq, Show, Generic)

instance FromXML ListAccessKeysResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ListAccessKeysResponse"
        :| ["ListAccessKeysResult"]
