{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.ListInvalidations
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | List invalidation batches.
module Network.AWS.CloudFront.ListInvalidations where

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
listInvalidations :: Text
                  -> Text
                  -> Text
                  -> ListInvalidations
listInvalidations p1 p2 p3 = ListInvalidations
    { lirDistributionId = p1
    , lirMarker = p2
    , lirMaxItems = p3
    }

data ListInvalidations = ListInvalidations
    { lirDistributionId :: !Text
      -- ^ The distribution's id.
    , lirMarker :: !Text
      -- ^ Use this parameter when paginating results to indicate where to begin in
      -- your list of invalidation batches. Because the results are returned in
      -- decreasing order from most recent to oldest, the most recent results are on
      -- the first page, the second page will contain earlier results, and so on. To
      -- get the next page of results, set the Marker to the value of the NextMarker
      -- from the current page's response. This value is the same as the ID of the
      -- last invalidation batch on that page.
    , lirMaxItems :: !Text
      -- ^ The maximum number of invalidation batches you want in the response body.
    } deriving (Eq, Show, Generic)

instance ToHeaders ListInvalidations

instance ToPath ListInvalidations where
    toPath ListInvalidations{..} = Text.concat
        [ "/2013-11-11/distribution/"
        , toText lirDistributionId
        , "/invalidation"
        ]

instance ToQuery ListInvalidations where
    toQuery ListInvalidations{..} = queryFromList
        [ "Marker" =? lirMarker
        , "MaxItems" =? lirMaxItems
        ]

instance ToXML ListInvalidations where
    toXMLOptions = xmlOptions

instance AWSRequest ListInvalidations where
    type Er ListInvalidations = CloudFrontError
    type Rs ListInvalidations = ListInvalidationsResponse
    request = getRestXML service

instance AWSPager ListInvalidations where
    next rq rs = undefined
--        | Just x <- lirrsNextMarker rs = Just $ rq { lirMarker = Just x }
--        | otherwise = Nothing

data ListInvalidationsResponse = ListInvalidationsResponse
    { lirrsInvalidationList :: Maybe InvalidationList
      -- ^ Information about invalidation batches.
    } deriving (Eq, Show, Generic)

instance FromXML ListInvalidationsResponse where
    fromXMLOptions = xmlOptions
