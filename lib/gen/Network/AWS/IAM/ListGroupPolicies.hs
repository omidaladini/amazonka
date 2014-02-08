{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ListGroupPolicies
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the names of the policies associated with the specified group. If
-- there are none, the action returns an empty list. You can paginate the
-- results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=ListGroupPolicies &GroupName=Admins
-- &AUTHPARAMS AdminRoot KeyPolicy false 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.ListGroupPolicies where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
listGroupPolicies :: Text
                  -- ^ The name of the group to list policies for.
                  -> ListGroupPolicies
listGroupPolicies p1 = ListGroupPolicies
    { lgprGroupName = p1
    , lgprMarker = Nothing
    , lgprMaxItems = Nothing
    }

data ListGroupPolicies = ListGroupPolicies
    { lgprGroupName :: !Text
      -- ^ The name of the group to list policies for.
    , lgprMarker :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent request
      -- after you've received a response where the results are truncated. Set it to
      -- the value of the Marker element in the response you just received.
    , lgprMaxItems :: Maybe Int
      -- ^ Use this only when paginating results to indicate the maximum number of
      -- policy names you want in the response. If there are additional policy names
      -- beyond the maximum you specify, the IsTruncated response element is true.
      -- This parameter is optional. If you do not include it, it defaults to 100.
    } deriving (Eq, Show, Generic)

instance ToQuery ListGroupPolicies

instance AWSRequest ListGroupPolicies where
    type Er ListGroupPolicies = IAMError
    type Rs ListGroupPolicies = ListGroupPoliciesResponse
    request = getQuery service "ListGroupPolicies"

instance AWSPager ListGroupPolicies where
    next rq rs
        | Just x <- lgprrsMarker rs = Just $ rq { lgprMarker = Just x }
        | otherwise = Nothing

data ListGroupPoliciesResponse = ListGroupPoliciesResponse
    { lgprrsIsTruncated :: Maybe Bool
      -- ^ A flag that indicates whether there are more policy names to list. If your
      -- results were truncated, you can make a subsequent pagination request using
      -- the Marker request parameter to retrieve more policy names in the list.
    , lgprrsMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the value to
      -- use for the Marker parameter in a subsequent pagination request.
    , lgprrsPolicyNames :: [Text]
      -- ^ A list of policy names.
    } deriving (Eq, Show, Generic)

instance FromXML ListGroupPoliciesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ListGroupPoliciesResponse"
        :| ["ListGroupPoliciesResult"]
