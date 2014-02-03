{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.ListStreamingDistributions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | List streaming distributions.
module Network.AWS.CloudFront.ListStreamingDistributions where

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
listStreamingDistributions :: Text
                           -> Text
                           -> ListStreamingDistributions
listStreamingDistributions p1 p2 = ListStreamingDistributions
    { lsdrMarker = p1
    , lsdrMaxItems = p2
    }

data ListStreamingDistributions = ListStreamingDistributions
    { lsdrMarker :: !Text
      -- ^ Use this when paginating results to indicate where to begin in your list of
      -- streaming distributions. The results include distributions in the list that
      -- occur after the marker. To get the next page of results, set the Marker to
      -- the value of the NextMarker from the current page's response (which is also
      -- the ID of the last distribution on that page).
    , lsdrMaxItems :: !Text
      -- ^ The maximum number of streaming distributions you want in the response
      -- body.
    } deriving (Eq, Show, Generic)

instance ToHeaders ListStreamingDistributions

instance ToPath ListStreamingDistributions where
    toPath = const "/2013-11-11/streaming-distribution"

instance ToQuery ListStreamingDistributions where
    toQuery ListStreamingDistributions{..} = queryFromList
        [ "Marker" =? lsdrMarker
        , "MaxItems" =? lsdrMaxItems
        ]

instance ToXML ListStreamingDistributions where
    toXMLOptions = xmlOptions

instance AWSRequest ListStreamingDistributions where
    type Er ListStreamingDistributions = CloudFrontError
    type Rs ListStreamingDistributions = ListStreamingDistributionsResponse
    request = getRestXML service

instance AWSPager ListStreamingDistributions where
    next rq rs = undefined
--        | Just x <- lsdrrsNextMarker rs = Just $ rq { lsdrMarker = Just x }
--        | otherwise = Nothing

data ListStreamingDistributionsResponse = ListStreamingDistributionsResponse
    { lsdrrsStreamingDistributionList :: Maybe StreamingDistributionList
      -- ^ The StreamingDistributionList type.
    } deriving (Eq, Show, Generic)

instance FromXML ListStreamingDistributionsResponse where
    fromXMLOptions = xmlOptions
