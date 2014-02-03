{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ListInstanceProfiles
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the instance profiles that have the specified path prefix. If there
-- are none, the action returns an empty list. For more information about
-- instance profiles, go to About Instance Profiles. You can paginate the
-- results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=ListInstanceProfiles &MaxItems=100
-- &PathPrefix=/application_abc/ &Version=2010-05-08 &AUTHPARAMS false
-- AIPACIFN4OZXG7EXAMPLE Database /application_abc/component_xyz/
-- arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Database
-- 2012-05-09T16:27:03Z AIPACZLSXM2EYYEXAMPLE Webserver
-- /application_abc/component_xyz/
-- arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Webserver
-- 2012-05-09T16:27:11Z fd74fa8d-99f3-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.ListInstanceProfiles where

import           Control.Monad
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import           Text.XML.Generic

import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
listInstanceProfiles :: ListInstanceProfiles
listInstanceProfiles = ListInstanceProfiles
    { liprMarker = Nothing
    , liprMaxItems = Nothing
    , liprPathPrefix = Nothing
    }

data ListInstanceProfiles = ListInstanceProfiles
    { liprMarker :: Maybe Text
      -- ^ Use this parameter only when paginating results, and only in a subsequent
      -- request after you've received a response where the results are truncated.
      -- Set it to the value of the Marker element in the response you just
      -- received.
    , liprMaxItems :: Maybe Int
      -- ^ Use this parameter only when paginating results to indicate the maximum
      -- number of user names you want in the response. If there are additional user
      -- names beyond the maximum you specify, the IsTruncated response element is
      -- true. This parameter is optional. If you do not include it, it defaults to
      -- 100.
    , liprPathPrefix :: Maybe Text
      -- ^ The path prefix for filtering the results. For example:
      -- /application_abc/component_xyz/, which would get all instance profiles
      -- whose path starts with /application_abc/component_xyz/. This parameter is
      -- optional. If it is not included, it defaults to a slash (/), listing all
      -- instance profiles.
    } deriving (Eq, Show, Generic)

instance ToQuery ListInstanceProfiles

instance AWSRequest ListInstanceProfiles where
    type Er ListInstanceProfiles = IAMError
    type Rs ListInstanceProfiles = ListInstanceProfilesResponse
    request = getQuery service "ListInstanceProfiles"

instance AWSPager ListInstanceProfiles where
    next rq rs
        | Just x <- liprrsMarker rs = Just $ rq { liprMarker = Just x }
        | otherwise = Nothing

data ListInstanceProfilesResponse = ListInstanceProfilesResponse
    { liprrsInstanceProfiles :: [InstanceProfile]
      -- ^ A list of instance profiles.
    , liprrsIsTruncated :: Maybe Bool
      -- ^ A flag that indicates whether there are more instance profiles to list. If
      -- your results were truncated, you can make a subsequent pagination request
      -- using the Marker request parameter to retrieve more instance profiles in
      -- the list.
    , liprrsMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the value to
      -- use for the Marker parameter in a subsequent pagination request.
    } deriving (Eq, Show, Generic)

instance FromXML ListInstanceProfilesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ListInstanceProfilesResponse"
        :| ["ListInstanceProfilesResult"]
