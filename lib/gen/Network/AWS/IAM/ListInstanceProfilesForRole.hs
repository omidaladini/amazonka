{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ListInstanceProfilesForRole
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the instance profiles that have the specified associated role. If
-- there are none, the action returns an empty list. For more information
-- about instance profiles, go to About Instance Profiles. You can paginate
-- the results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=ListInstanceProfilesForRole
-- &MaxItems=100 &RoleName=S3Access &Version=2010-05-08 &AUTHPARAMS false
-- AIPACZLS2EYYXMEXAMPLE /application_abc/component_xyz/
-- arn:aws:iam::123456789012:role/application_abc/component_xyz/S3Access
-- S3Access
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":["ec2.amazonaws.com"]},"Action":["sts:AssumeRole"]}]}
-- 2012-05-09T15:45:35Z AROACVSVTSZYK3EXAMPLE Webserver
-- /application_abc/component_xyz/
-- arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Webserver
-- 2012-05-09T16:27:11Z 6a8c3992-99f4-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.ListInstanceProfilesForRole where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
listInstanceProfilesForRole :: Text
                            -- ^ The name of the role to list instance profiles for.
                            -> ListInstanceProfilesForRole
listInstanceProfilesForRole p1 = ListInstanceProfilesForRole
    { lipfrRoleName = p1
    , lipfrMarker = Nothing
    , lipfrMaxItems = Nothing
    }

data ListInstanceProfilesForRole = ListInstanceProfilesForRole
    { lipfrMarker :: Maybe Text
      -- ^ Use this parameter only when paginating results, and only in a subsequent
      -- request after you've received a response where the results are truncated.
      -- Set it to the value of the Marker element in the response you just
      -- received.
    , lipfrMaxItems :: Maybe Int
      -- ^ Use this parameter only when paginating results to indicate the maximum
      -- number of user names you want in the response. If there are additional user
      -- names beyond the maximum you specify, the IsTruncated response element is
      -- true. This parameter is optional. If you do not include it, it defaults to
      -- 100.
    , lipfrRoleName :: !Text
      -- ^ The name of the role to list instance profiles for.
    } deriving (Eq, Show, Generic)

instance ToQuery ListInstanceProfilesForRole

instance AWSRequest ListInstanceProfilesForRole where
    type Er ListInstanceProfilesForRole = IAMError
    type Rs ListInstanceProfilesForRole = ListInstanceProfilesForRoleResponse
    request  = postQuery service "ListInstanceProfilesForRole"
    response = responseXML

instance AWSPager ListInstanceProfilesForRole where
    next rq rs
        | Just x <- lipfrrMarker rs = Just $ rq { lipfrMarker = Just x }
        | otherwise = Nothing

data ListInstanceProfilesForRoleResponse = ListInstanceProfilesForRoleResponse
    { lipfrrInstanceProfiles :: [InstanceProfile]
      -- ^ A list of instance profiles.
    , lipfrrIsTruncated :: Maybe Bool
      -- ^ A flag that indicates whether there are more instance profiles to list. If
      -- your results were truncated, you can make a subsequent pagination request
      -- using the Marker request parameter to retrieve more instance profiles in
      -- the list.
    , lipfrrMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the value to
      -- use for the Marker parameter in a subsequent pagination request.
    } deriving (Eq, Show, Generic)

instance FromXML ListInstanceProfilesForRoleResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ListInstanceProfilesForRoleResponse"
        :| ["ListInstanceProfilesForRoleResult"]
