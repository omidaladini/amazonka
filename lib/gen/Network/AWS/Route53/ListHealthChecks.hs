{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.ListHealthChecks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | To retrieve a list of your health checks, send a GET request to the
-- 2012-12-12/healthcheck resource. The response to this request includes a
-- HealthChecks element with zero, one, or multiple HealthCheck child
-- elements. By default, the list of health checks is displayed on a single
-- page. You can control the length of the page that is displayed by using the
-- MaxItems parameter. You can use the Marker parameter to control the health
-- check that the list begins with. Amazon Route 53 returns a maximum of 100
-- items. If you set MaxItems to a value greater than 100, Amazon Route 53
-- returns only the first 100.
module Network.AWS.Route53.ListHealthChecks where

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

-- | Convenience method utilising default fields where applicable.
listHealthChecks :: AWS (Either Route53Error ListHealthChecksResponse)
listHealthChecks = undefined $ ListHealthChecks
    { lhcrMarker = Nothing
    , lhcrMaxItems = Nothing
    }

data ListHealthChecks = ListHealthChecks
    { lhcrMarker :: Maybe Text
      -- ^ If the request returned more than one page of results, submit another
      -- request and specify the value of NextMarker from the last response in the
      -- marker parameter to get the next page of results.
    , lhcrMaxItems :: Maybe Text
      -- ^ Specify the maximum number of health checks to return per page of results.
    } deriving (Eq, Show, Generic)

instance ToHeaders ListHealthChecks

instance ToPath ListHealthChecks where
    toPath = const "/2012-12-12/healthcheck"

instance ToQuery ListHealthChecks where
    toQuery ListHealthChecks{..} = List
        [ "marker" =? lhcrMarker
        , "maxitems" =? lhcrMaxItems
        ]

instance ToXML ListHealthChecks where
    toXMLOptions = xmlOptions

instance AWSRequest ListHealthChecks where
    type Er ListHealthChecks = Route53Error
    type Rs ListHealthChecks = ListHealthChecksResponse
    request = getRestXML service

data ListHealthChecksResponse = ListHealthChecksResponse
    { lhcrrsHealthChecks :: [HealthCheck]
      -- ^ A complex type that contains information about the health checks associated
      -- with the current AWS account.
    , lhcrrsIsTruncated :: !Bool
      -- ^ A flag indicating whether there are more health checks to be listed. If
      -- your results were truncated, you can make a follow-up request for the next
      -- page of results by using the Marker element. Valid Values: true | false.
    , lhcrrsMarker :: !Text
      -- ^ If the request returned more than one page of results, submit another
      -- request and specify the value of NextMarker from the last response in the
      -- marker parameter to get the next page of results.
    , lhcrrsMaxItems :: !Text
      -- ^ The maximum number of health checks to be included in the response body. If
      -- the number of health checks associated with this AWS account exceeds
      -- MaxItems, the value of ListHealthChecksResponse$IsTruncated in the response
      -- is true. Call ListHealthChecks again and specify the value of
      -- ListHealthChecksResponse$NextMarker in the ListHostedZonesRequest$Marker
      -- element to get the next page of results.
    , lhcrrsNextMarker :: Maybe Text
      -- ^ Indicates where to continue listing health checks. If
      -- ListHealthChecksResponse$IsTruncated is true, make another request to
      -- ListHealthChecks and include the value of the NextMarker element in the
      -- Marker element to get the next page of results.
    } deriving (Eq, Show, Generic)

instance FromXML ListHealthChecksResponse where
    fromXMLOptions = xmlOptions
