{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.ListDistributions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | List distributions.
module Network.AWS.CloudFront.ListDistributions where

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
listDistributions :: Text
                  -> Text
                  -> ListDistributions
listDistributions p1 p2 = ListDistributions
    { ldrMarker = p1
    , ldrMaxItems = p2
    }

data ListDistributions = ListDistributions
    { ldrMarker :: !Text
      -- ^ Use this when paginating results to indicate where to begin in your list of
      -- distributions. The results include distributions in the list that occur
      -- after the marker. To get the next page of results, set the Marker to the
      -- value of the NextMarker from the current page's response (which is also the
      -- ID of the last distribution on that page).
    , ldrMaxItems :: !Text
      -- ^ The maximum number of distributions you want in the response body.
    } deriving (Eq, Show, Generic)

instance ToHeaders ListDistributions

instance ToPath ListDistributions where
    toPath = const "/2013-11-11/distribution"

instance ToQuery ListDistributions where
    toQuery ListDistributions{..} = queryFromList
        [ "Marker" =? ldrMarker
        , "MaxItems" =? ldrMaxItems
        ]

instance ToXML ListDistributions where
    toXMLOptions = xmlOptions

instance AWSRequest ListDistributions where
    type Er ListDistributions = CloudFrontError
    type Rs ListDistributions = ListDistributionsResponse
    request = getRestXML service

instance AWSPager ListDistributions where
    next rq rs = undefined
--        | Just x <- ldrrsNextMarker rs = Just $ rq { ldrMarker = Just x }
--        | otherwise = Nothing

data ListDistributionsResponse = ListDistributionsResponse
    { ldrrsDistributionList :: Maybe DistributionList
      -- ^ The DistributionList type.
    } deriving (Eq, Show, Generic)

instance FromXML ListDistributionsResponse where
    fromXMLOptions = xmlOptions
