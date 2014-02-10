{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ListGroupsForUser
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the groups the specified user belongs to. You can paginate the
-- results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=ListGroupsForUser &UserName=Bob
-- &AUTHPARAMS / Admins AGPACKCEVSQ6C2EXAMPLE
-- arn:aws:iam::123456789012:group/Admins false
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.ListGroupsForUser where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
listGroupsForUser :: Text
                  -- ^ The name of the user to list groups for.
                  -> ListGroupsForUser
listGroupsForUser p1 = ListGroupsForUser
    { lgfuUserName = p1
    , lgfuMarker = Nothing
    , lgfuMaxItems = Nothing
    }

data ListGroupsForUser = ListGroupsForUser
    { lgfuMarker :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent request
      -- after you've received a response where the results are truncated. Set it to
      -- the value of the Marker element in the response you just received.
    , lgfuMaxItems :: Maybe Int
      -- ^ Use this only when paginating results to indicate the maximum number of
      -- groups you want in the response. If there are additional groups beyond the
      -- maximum you specify, the IsTruncated response element is true. This
      -- parameter is optional. If you do not include it, it defaults to 100.
    , lgfuUserName :: !Text
      -- ^ The name of the user to list groups for.
    } deriving (Eq, Show, Generic)

instance ToQuery ListGroupsForUser

instance AWSRequest ListGroupsForUser where
    type Er ListGroupsForUser = IAMError
    type Rs ListGroupsForUser = ListGroupsForUserResponse
    request  = postQuery service "ListGroupsForUser"
    response = responseXML

instance AWSPager ListGroupsForUser where
    next rq rs
        | Just x <- lgfurMarker rs = Just $ rq { lgfuMarker = Just x }
        | otherwise = Nothing

data ListGroupsForUserResponse = ListGroupsForUserResponse
    { lgfurGroups :: [Group]
      -- ^ A list of groups.
    , lgfurIsTruncated :: Maybe Bool
      -- ^ A flag that indicates whether there are more groups to list. If your
      -- results were truncated, you can make a subsequent pagination request using
      -- the Marker request parameter to retrieve more groups in the list.
    , lgfurMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the value to
      -- use for the Marker parameter in a subsequent pagination request.
    } deriving (Eq, Show, Generic)

instance FromXML ListGroupsForUserResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ListGroupsForUserResponse"
        :| ["ListGroupsForUserResult"]
