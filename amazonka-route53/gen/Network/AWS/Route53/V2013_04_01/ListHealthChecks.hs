{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.V2013_04_01.ListHealthChecks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | To retrieve a list of your health checks, send a GET request to the
-- 2013-04-01/healthcheck resource. The response to this request includes a
-- HealthChecks element with zero, one, or multiple HealthCheck child
-- elements. By default, the list of health checks is displayed on a single
-- page. You can control the length of the page that is displayed by using the
-- MaxItems parameter. You can use the Marker parameter to control the health
-- check that the list begins with. Amazon Route 53 returns a maximum of 100
-- items. If you set MaxItems to a value greater than 100, Amazon Route 53
-- returns only the first 100.
module Network.AWS.Route53.V2013_04_01.ListHealthChecks
    (
    -- * Request
      ListHealthChecks
    -- ** Request constructor
    , listHealthChecks
    -- ** Request lenses
    , lhcrMarker
    , lhcrMaxItems

    -- * Response
    , ListHealthChecksResponse
    -- ** Response lenses
    , lhcsHealthChecks
    , lhcsMarker
    , lhcsMaxItems
    , lhcsIsTruncated
    , lhcsNextMarker
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListHealthChecks' request.
listHealthChecks :: ListHealthChecks
listHealthChecks = ListHealthChecks
    { _lhcrMarker = Nothing
    , _lhcrMaxItems = Nothing
    }

data ListHealthChecks = ListHealthChecks
    { _lhcrMarker :: Maybe Text
      -- ^ If the request returned more than one page of results, submit
      -- another request and specify the value of NextMarker from the last
      -- response in the marker parameter to get the next page of results.
    , _lhcrMaxItems :: Maybe Text
      -- ^ Specify the maximum number of health checks to return per page of
      -- results.
    } deriving (Show, Generic)

-- | If the request returned more than one page of results, submit another
-- request and specify the value of NextMarker from the last response in the
-- marker parameter to get the next page of results.
lhcrMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListHealthChecks
    -> f ListHealthChecks
lhcrMarker f x =
    (\y -> x { _lhcrMarker = y })
       <$> f (_lhcrMarker x)
{-# INLINE lhcrMarker #-}

-- | Specify the maximum number of health checks to return per page of results.
lhcrMaxItems
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListHealthChecks
    -> f ListHealthChecks
lhcrMaxItems f x =
    (\y -> x { _lhcrMaxItems = y })
       <$> f (_lhcrMaxItems x)
{-# INLINE lhcrMaxItems #-}

instance ToPath ListHealthChecks where
    toPath = const "/2013-04-01/healthcheck"

instance ToQuery ListHealthChecks where
    toQuery ListHealthChecks{..} = mconcat
        [ "marker" =? _lhcrMarker
        , "maxitems" =? _lhcrMaxItems
        ]

instance ToHeaders ListHealthChecks

instance ToXML ListHealthChecks where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ListHealthChecksRequest"

data ListHealthChecksResponse = ListHealthChecksResponse
    { _lhcsHealthChecks :: [HealthCheck]
      -- ^ A complex type that contains information about the health checks
      -- associated with the current AWS account.
    , _lhcsMarker :: Text
      -- ^ If the request returned more than one page of results, submit
      -- another request and specify the value of NextMarker from the last
      -- response in the marker parameter to get the next page of results.
    , _lhcsMaxItems :: Text
      -- ^ The maximum number of health checks to be included in the
      -- response body. If the number of health checks associated with
      -- this AWS account exceeds MaxItems, the value of
      -- ListHealthChecksResponse$IsTruncated in the response is true.
      -- Call ListHealthChecks again and specify the value of
      -- ListHealthChecksResponse$NextMarker in the
      -- ListHostedZonesRequest$Marker element to get the next page of
      -- results.
    , _lhcsIsTruncated :: Bool
      -- ^ A flag indicating whether there are more health checks to be
      -- listed. If your results were truncated, you can make a follow-up
      -- request for the next page of results by using the Marker element.
      -- Valid Values: true | false.
    , _lhcsNextMarker :: Maybe Text
      -- ^ Indicates where to continue listing health checks. If
      -- ListHealthChecksResponse$IsTruncated is true, make another
      -- request to ListHealthChecks and include the value of the
      -- NextMarker element in the Marker element to get the next page of
      -- results.
    } deriving (Show, Generic)

-- | A complex type that contains information about the health checks associated
-- with the current AWS account.
lhcsHealthChecks
    :: Functor f
    => ([HealthCheck]
    -> f ([HealthCheck]))
    -> ListHealthChecksResponse
    -> f ListHealthChecksResponse
lhcsHealthChecks f x =
    (\y -> x { _lhcsHealthChecks = y })
       <$> f (_lhcsHealthChecks x)
{-# INLINE lhcsHealthChecks #-}

-- | If the request returned more than one page of results, submit another
-- request and specify the value of NextMarker from the last response in the
-- marker parameter to get the next page of results.
lhcsMarker
    :: Functor f
    => (Text
    -> f (Text))
    -> ListHealthChecksResponse
    -> f ListHealthChecksResponse
lhcsMarker f x =
    (\y -> x { _lhcsMarker = y })
       <$> f (_lhcsMarker x)
{-# INLINE lhcsMarker #-}

-- | The maximum number of health checks to be included in the response body. If
-- the number of health checks associated with this AWS account exceeds
-- MaxItems, the value of ListHealthChecksResponse$IsTruncated in the response
-- is true. Call ListHealthChecks again and specify the value of
-- ListHealthChecksResponse$NextMarker in the ListHostedZonesRequest$Marker
-- element to get the next page of results.
lhcsMaxItems
    :: Functor f
    => (Text
    -> f (Text))
    -> ListHealthChecksResponse
    -> f ListHealthChecksResponse
lhcsMaxItems f x =
    (\y -> x { _lhcsMaxItems = y })
       <$> f (_lhcsMaxItems x)
{-# INLINE lhcsMaxItems #-}

-- | A flag indicating whether there are more health checks to be listed. If
-- your results were truncated, you can make a follow-up request for the next
-- page of results by using the Marker element. Valid Values: true | false.
lhcsIsTruncated
    :: Functor f
    => (Bool
    -> f (Bool))
    -> ListHealthChecksResponse
    -> f ListHealthChecksResponse
lhcsIsTruncated f x =
    (\y -> x { _lhcsIsTruncated = y })
       <$> f (_lhcsIsTruncated x)
{-# INLINE lhcsIsTruncated #-}

-- | Indicates where to continue listing health checks. If
-- ListHealthChecksResponse$IsTruncated is true, make another request to
-- ListHealthChecks and include the value of the NextMarker element in the
-- Marker element to get the next page of results.
lhcsNextMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListHealthChecksResponse
    -> f ListHealthChecksResponse
lhcsNextMarker f x =
    (\y -> x { _lhcsNextMarker = y })
       <$> f (_lhcsNextMarker x)
{-# INLINE lhcsNextMarker #-}

instance FromXML ListHealthChecksResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListHealthChecks where
    type Sv ListHealthChecks = Route53
    type Rs ListHealthChecks = ListHealthChecksResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListHealthChecks where
    next rq rs
        | not (_lhcsIsTruncated rs) = Nothing
        | otherwise = Just $ rq
            { _lhcrMarker = _lhcsNextMarker rs
            }
