{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.ListHostedZones
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | To retrieve a list of your hosted zones, send a GET request to the
-- 2012-12-12/hostedzone resource. The response to this request includes a
-- HostedZones element with zero, one, or multiple HostedZone child elements.
-- By default, the list of hosted zones is displayed on a single page. You can
-- control the length of the page that is displayed by using the MaxItems
-- parameter. You can use the Marker parameter to control the hosted zone that
-- the list begins with. Amazon Route 53 returns a maximum of 100 items. If
-- you set MaxItems to a value greater than 100, Amazon Route 53 returns only
-- the first 100.
module Network.AWS.Route53.ListHostedZones where

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

import Network.AWS.Route53.Service
import Network.AWS.Route53.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
listHostedZones :: ListHostedZones
listHostedZones = ListHostedZones
    { lhzrMarker = Nothing
    , lhzrMaxItems = Nothing
    }

data ListHostedZones = ListHostedZones
    { lhzrMarker :: Maybe Text
      -- ^ If the request returned more than one page of results, submit another
      -- request and specify the value of NextMarker from the last response in the
      -- marker parameter to get the next page of results.
    , lhzrMaxItems :: Maybe Text
      -- ^ Specify the maximum number of hosted zones to return per page of results.
    } deriving (Eq, Show, Generic)

instance ToHeaders ListHostedZones

instance ToPath ListHostedZones where
    toPath = const "/2012-12-12/hostedzone"

instance ToQuery ListHostedZones where
    toQuery ListHostedZones{..} = List
        [ "marker" =? lhzrMarker
        , "maxitems" =? lhzrMaxItems
        ]

instance ToXML ListHostedZones where
    toXMLOptions = xmlOptions

instance AWSRequest ListHostedZones where
    type Er ListHostedZones = Route53Error
    type Rs ListHostedZones = ListHostedZonesResponse
    request = getRestXML service

data ListHostedZonesResponse = ListHostedZonesResponse
    { lhzrrsHostedZones :: [HostedZone]
      -- ^ A complex type that contains information about the hosted zones associated
      -- with the current AWS account.
    , lhzrrsIsTruncated :: !Bool
      -- ^ A flag indicating whether there are more hosted zones to be listed. If your
      -- results were truncated, you can make a follow-up request for the next page
      -- of results by using the Marker element. Valid Values: true | false.
    , lhzrrsMarker :: !Text
      -- ^ If the request returned more than one page of results, submit another
      -- request and specify the value of NextMarker from the last response in the
      -- marker parameter to get the next page of results.
    , lhzrrsMaxItems :: !Text
      -- ^ The maximum number of hosted zones to be included in the response body. If
      -- the number of hosted zones associated with this AWS account exceeds
      -- MaxItems, the value of ListHostedZonesResponse$IsTruncated in the response
      -- is true. Call ListHostedZones again and specify the value of
      -- ListHostedZonesResponse$NextMarker in the ListHostedZonesRequest$Marker
      -- element to get the next page of results.
    , lhzrrsNextMarker :: Maybe Text
      -- ^ Indicates where to continue listing hosted zones. If
      -- ListHostedZonesResponse$IsTruncated is true, make another request to
      -- ListHostedZones and include the value of the NextMarker element in the
      -- Marker element to get the next page of results.
    } deriving (Eq, Show, Generic)

instance FromXML ListHostedZonesResponse where
    fromXMLOptions = xmlOptions
