{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.GetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of users that are in the specified group. You can paginate
-- the results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=GetGroup &GroupName=Admins
-- &Version=2010-05-08 &AUTHPARAMS / Admins AGPACKCEVSQ6C2EXAMPLE
-- arn:aws:iam::123456789012:group/Admins /division_abc/subdivision_xyz/ Bob
-- AIDACKCEVSQ6C2EXAMPLE
-- arn:aws:iam::123456789012:user/division_abc/subdivision_xyz/Bob
-- /division_abc/subdivision_xyz/ Susan AIDACKCEVSQ6C2EXAMPLE
-- arn:aws:iam::123456789012:user/division_abc/subdivision_xyz/Susan false
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.GetGroup where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
getGroup :: Text
         -- ^ Name of the group.
         -> GetGroup
getGroup p1 = GetGroup
    { ggGroupName = p1
    , ggMarker = Nothing
    , ggMaxItems = Nothing
    }

data GetGroup = GetGroup
    { ggGroupName :: !Text
      -- ^ Name of the group.
    , ggMarker :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent request
      -- after you've received a response where the results are truncated. Set it to
      -- the value of the Marker element in the response you just received.
    , ggMaxItems :: Maybe Int
      -- ^ Use this only when paginating results to indicate the maximum number of
      -- user names you want in the response. If there are additional user names
      -- beyond the maximum you specify, the IsTruncated response element is true.
      -- This parameter is optional. If you do not include it, it defaults to 100.
    } deriving (Eq, Show, Generic)

instance ToQuery GetGroup

instance AWSRequest GetGroup where
    type Er GetGroup = IAMError
    type Rs GetGroup = GetGroupResponse
    request  = postQuery service "GetGroup"
    response = responseXML

instance AWSPager GetGroup where
    next rq rs
        | Just x <- ggrMarker rs = Just $ rq { ggMarker = Just x }
        | otherwise = Nothing

data GetGroupResponse = GetGroupResponse
    { ggrGroup :: Group
      -- ^ Information about the group.
    , ggrIsTruncated :: Maybe Bool
      -- ^ A flag that indicates whether there are more user names to list. If your
      -- results were truncated, you can make a subsequent pagination request using
      -- the Marker request parameter to retrieve more user names in the list.
    , ggrMarker :: Maybe Text
      -- ^ If IsTruncated is true, then this element is present and contains the value
      -- to use for the Marker parameter in a subsequent pagination request.
    , ggrUsers :: [User]
      -- ^ A list of users in the group.
    } deriving (Eq, Show, Generic)

instance FromXML GetGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetGroupResponse"
        :| ["GetGroupResult"]
