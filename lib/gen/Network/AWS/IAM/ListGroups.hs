{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ListGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the groups that have the specified path prefix. You can paginate the
-- results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=ListGroups
-- &PathPrefix=/division_abc/subdivision_xyz/ &Version=2010-05-08 &AUTHPARAMS
-- /division_abc/subdivision_xyz/ Admins AGPACKCEVSQ6C2EXAMPLE
-- arn:aws:iam::123456789012:group/Admins
-- /division_abc/subdivision_xyz/product_1234/engineering/ Test
-- AGP2MAB8DPLSRHEXAMPLE arn:aws:iam::123456789012:group
-- /division_abc/subdivision_xyz/product_1234/engineering/Test
-- /division_abc/subdivision_xyz/product_1234/ Managers AGPIODR4TAW7CSEXAMPLE
-- arn:aws:iam::123456789012
-- :group/division_abc/subdivision_xyz/product_1234/Managers false
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.ListGroups where

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

-- | Convenience method utilising default fields where applicable.
listGroups :: AWS (Either IAMError ListGroupsResponse)
listGroups = undefined $ ListGroups
    { lgrMarker = Nothing
    , lgrMaxItems = Nothing
    , lgrPathPrefix = Nothing
    }

data ListGroups = ListGroups
    { lgrMarker :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent request
      -- after you've received a response where the results are truncated. Set it to
      -- the value of the Marker element in the response you just received.
    , lgrMaxItems :: Maybe Int
      -- ^ Use this only when paginating results to indicate the maximum number of
      -- groups you want in the response. If there are additional groups beyond the
      -- maximum you specify, the IsTruncated response element is true. This
      -- parameter is optional. If you do not include it, it defaults to 100.
    , lgrPathPrefix :: Maybe Text
      -- ^ The path prefix for filtering the results. For example:
      -- /division_abc/subdivision_xyz/, which would get all groups whose path
      -- starts with /division_abc/subdivision_xyz/. This parameter is optional. If
      -- it is not included, it defaults to a slash (/), listing all groups.
    } deriving (Eq, Show, Generic)

instance ToQuery ListGroups

instance AWSRequest ListGroups where
    type Er ListGroups = IAMError
    type Rs ListGroups = ListGroupsResponse
    request = getQuery service "ListGroups"

instance AWSPager ListGroups where
    next rq rs
        | Just x <- lgrrsMarker rs = Just $ rq { lgrMarker = Just x }
        | otherwise = Nothing

data ListGroupsResponse = ListGroupsResponse
    { lgrrsGroups :: [Group]
      -- ^ A list of groups.
    , lgrrsIsTruncated :: Maybe Bool
      -- ^ A flag that indicates whether there are more groups to list. If your
      -- results were truncated, you can make a subsequent pagination request using
      -- the Marker request parameter to retrieve more groups in the list.
    , lgrrsMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the value to
      -- use for the Marker parameter in a subsequent pagination request.
    } deriving (Eq, Show, Generic)

instance FromXML ListGroupsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ListGroupsResponse"
        :| ["ListGroupsResult"]
