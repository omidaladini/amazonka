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

data ListGroupsForUser = ListGroupsForUser
    { lgfurMarker :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent request
      -- after you've received a response where the results are truncated. Set it to
      -- the value of the Marker element in the response you just received.
    , lgfurMaxItems :: Maybe Int
      -- ^ Use this only when paginating results to indicate the maximum number of
      -- groups you want in the response. If there are additional groups beyond the
      -- maximum you specify, the IsTruncated response element is true. This
      -- parameter is optional. If you do not include it, it defaults to 100.
    , lgfurUserName :: !Text
      -- ^ The name of the user to list groups for.
    } deriving (Eq, Show, Generic)

instance ToQuery ListGroupsForUser

instance AWSRequest ListGroupsForUser where
    type Er ListGroupsForUser = IAMError
    type Rs ListGroupsForUser = ListGroupsForUserResponse
    request = getQuery service "ListGroupsForUser"

instance AWSPager ListGroupsForUser where
    next rq rs
        | Just x <- lgfurrsMarker rs = Just $ rq { lgfurMarker = Just x }
        | otherwise = Nothing

data ListGroupsForUserResponse = ListGroupsForUserResponse
    { lgfurrsGroups :: [Group]
      -- ^ A list of groups.
    , lgfurrsIsTruncated :: Maybe Bool
      -- ^ A flag that indicates whether there are more groups to list. If your
      -- results were truncated, you can make a subsequent pagination request using
      -- the Marker request parameter to retrieve more groups in the list.
    , lgfurrsMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the value to
      -- use for the Marker parameter in a subsequent pagination request.
    } deriving (Eq, Show, Generic)

instance FromXML ListGroupsForUserResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ListGroupsForUserResponse"
        :| ["ListGroupsForUserResult"]