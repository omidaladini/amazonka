{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ListUserPolicies
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the names of the policies associated with the specified user. If
-- there are none, the action returns an empty list. You can paginate the
-- results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=ListUserPolicies &UserName=Bob
-- &AUTHPARAMS AllAccessPolicy KeyPolicy false
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.ListUserPolicies where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
listUserPolicies :: Text
                 -- ^ The name of the user to list policies for.
                 -> ListUserPolicies
listUserPolicies p1 = ListUserPolicies
    { luprUserName = p1
    , luprMarker = Nothing
    , luprMaxItems = Nothing
    }

data ListUserPolicies = ListUserPolicies
    { luprMarker :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent request
      -- after you've received a response where the results are truncated. Set it to
      -- the value of the Marker element in the response you just received.
    , luprMaxItems :: Maybe Int
      -- ^ Use this only when paginating results to indicate the maximum number of
      -- policy names you want in the response. If there are additional policy names
      -- beyond the maximum you specify, the IsTruncated response element is true.
      -- This parameter is optional. If you do not include it, it defaults to 100.
    , luprUserName :: !Text
      -- ^ The name of the user to list policies for.
    } deriving (Eq, Show, Generic)

instance ToQuery ListUserPolicies

instance AWSRequest ListUserPolicies where
    type Er ListUserPolicies = IAMError
    type Rs ListUserPolicies = ListUserPoliciesResponse
    request = getQuery service "ListUserPolicies"

instance AWSPager ListUserPolicies where
    next rq rs
        | Just x <- luprrsMarker rs = Just $ rq { luprMarker = Just x }
        | otherwise = Nothing

data ListUserPoliciesResponse = ListUserPoliciesResponse
    { luprrsIsTruncated :: Maybe Bool
      -- ^ A flag that indicates whether there are more policy names to list. If your
      -- results were truncated, you can make a subsequent pagination request using
      -- the Marker request parameter to retrieve more policy names in the list.
    , luprrsMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the value to
      -- use for the Marker parameter in a subsequent pagination request.
    , luprrsPolicyNames :: [Text]
      -- ^ A list of policy names.
    } deriving (Eq, Show, Generic)

instance FromXML ListUserPoliciesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ListUserPoliciesResponse"
        :| ["ListUserPoliciesResult"]
