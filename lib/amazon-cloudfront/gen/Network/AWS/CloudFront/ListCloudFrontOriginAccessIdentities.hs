{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | List origin access identities.
module Network.AWS.CloudFront.ListCloudFrontOriginAccessIdentities where

import           Control.Monad
import           Data.ByteString                  (ByteString)
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as Map
import           Data.List.NonEmpty               (NonEmpty(..))
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Time                        (UTCTime)
import           GHC.Generics                     (Generic)
import           Network.AWS.Internal             hiding (Endpoint, Region, AvailabilityZone)
import           Network.HTTP.QueryString.Generic (Query(List))

import Network.AWS.CloudFront.Service
import Network.AWS.CloudFront.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
listCloudFrontOriginAccessIdentities :: Text
                                     -> Text
                                     -> ListCloudFrontOriginAccessIdentities
listCloudFrontOriginAccessIdentities p1 p2 = ListCloudFrontOriginAccessIdentities
    { lcfoairMarker = p1
    , lcfoairMaxItems = p2
    }

data ListCloudFrontOriginAccessIdentities = ListCloudFrontOriginAccessIdentities
    { lcfoairMarker :: !Text
      -- ^ Use this when paginating results to indicate where to begin in your list of
      -- origin access identities. The results include identities in the list that
      -- occur after the marker. To get the next page of results, set the Marker to
      -- the value of the NextMarker from the current page's response (which is also
      -- the ID of the last identity on that page).
    , lcfoairMaxItems :: !Text
      -- ^ The maximum number of origin access identities you want in the response
      -- body.
    } deriving (Eq, Show, Generic)

instance ToHeaders ListCloudFrontOriginAccessIdentities

instance ToPath ListCloudFrontOriginAccessIdentities where
    toPath = const "/2013-11-11/origin-access-identity/cloudfront"

instance ToQuery ListCloudFrontOriginAccessIdentities where
    toQuery ListCloudFrontOriginAccessIdentities{..} = queryFromList
        [ "Marker" =? lcfoairMarker
        , "MaxItems" =? lcfoairMaxItems
        ]

instance ToXML ListCloudFrontOriginAccessIdentities where
    toXMLOptions = xmlOptions

instance AWSRequest ListCloudFrontOriginAccessIdentities where
    type Er ListCloudFrontOriginAccessIdentities = CloudFrontError
    type Rs ListCloudFrontOriginAccessIdentities = ListCloudFrontOriginAccessIdentitiesResponse
    request = getRestXML service

instance AWSPager ListCloudFrontOriginAccessIdentities where
    next rq rs = undefined
--        | Just x <- lcfoairrsNextMarker rs = Just $ rq { lcfoairMarker = Just x }
--        | otherwise = Nothing

data ListCloudFrontOriginAccessIdentitiesResponse = ListCloudFrontOriginAccessIdentitiesResponse
    { lcfoairrsCloudFrontOriginAccessIdentityList :: Maybe CloudFrontOriginAccessIdentityList
      -- ^ The CloudFrontOriginAccessIdentityList type.
    } deriving (Eq, Show, Generic)

instance FromXML ListCloudFrontOriginAccessIdentitiesResponse where
    fromXMLOptions = xmlOptions
