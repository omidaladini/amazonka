{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ListRolePolicies
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the names of the policies associated with the specified role. If
-- there are none, the action returns an empty list. You can paginate the
-- results using the MaxItems and Marker parameters.
-- https://iam.amazonaws.com/ ?Action=ListRolePolicies &MaxItems=100
-- &RoleName=S3Access &Version=2010-05-08 &AUTHPARAMS
-- CloudwatchPutMetricPolicy S3AccessPolicy false
-- 8c7e1816-99f0-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.ListRolePolicies where

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
listRolePolicies :: Text
                 -> ListRolePolicies
listRolePolicies p1 = ListRolePolicies
    { lrprRoleName = p1
    , lrprMarker = Nothing
    , lrprMaxItems = Nothing
    }

data ListRolePolicies = ListRolePolicies
    { lrprMarker :: Maybe Text
      -- ^ Use this parameter only when paginating results, and only in a subsequent
      -- request after you've received a response where the results are truncated.
      -- Set it to the value of the Marker element in the response you just
      -- received.
    , lrprMaxItems :: Maybe Int
      -- ^ Use this parameter only when paginating results to indicate the maximum
      -- number of user names you want in the response. If there are additional user
      -- names beyond the maximum you specify, the IsTruncated response element is
      -- true. This parameter is optional. If you do not include it, it defaults to
      -- 100.
    , lrprRoleName :: !Text
      -- ^ The name of the role to list policies for.
    } deriving (Eq, Show, Generic)

instance ToQuery ListRolePolicies

instance AWSRequest ListRolePolicies where
    type Er ListRolePolicies = IAMError
    type Rs ListRolePolicies = ListRolePoliciesResponse
    request = getQuery service "ListRolePolicies"

instance AWSPager ListRolePolicies where
    next rq rs
        | Just x <- lrprrsMarker rs = Just $ rq { lrprMarker = Just x }
        | otherwise = Nothing

data ListRolePoliciesResponse = ListRolePoliciesResponse
    { lrprrsIsTruncated :: Maybe Bool
      -- ^ A flag that indicates whether there are more policy names to list. If your
      -- results were truncated, you can make a subsequent pagination request using
      -- the Marker request parameter to retrieve more policy names in the list.
    , lrprrsMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the value to
      -- use for the Marker parameter in a subsequent pagination request.
    , lrprrsPolicyNames :: [Text]
      -- ^ A list of policy names.
    } deriving (Eq, Show, Generic)

instance FromXML ListRolePoliciesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ListRolePoliciesResponse"
        :| ["ListRolePoliciesResult"]
