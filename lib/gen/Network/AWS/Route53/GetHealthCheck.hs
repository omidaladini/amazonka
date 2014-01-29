{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.GetHealthCheck
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | To retrieve the health check, send a GET request to the
-- 2012-12-12/healthcheck/health check ID resource.
module Network.AWS.Route53.GetHealthCheck where

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
getHealthCheck :: Text
               -> GetHealthCheck
getHealthCheck p1 = undefined $ GetHealthCheck
    { ghcrHealthCheckId = p1
    }

data GetHealthCheck = GetHealthCheck
    { ghcrHealthCheckId :: !Text
      -- ^ The ID of the health check to retrieve.
    } deriving (Eq, Show, Generic)

instance ToHeaders GetHealthCheck

instance ToPath GetHealthCheck where
    toPath GetHealthCheck{..} = Text.concat
        [ "/2012-12-12/healthcheck/"
        , toText ghcrHealthCheckId
        ]

instance ToQuery GetHealthCheck where
    toQuery = const mempty

instance ToXML GetHealthCheck where
    toXMLOptions = xmlOptions

instance AWSRequest GetHealthCheck where
    type Er GetHealthCheck = Route53Error
    type Rs GetHealthCheck = GetHealthCheckResponse
    request = getRestXML service

data GetHealthCheckResponse = GetHealthCheckResponse
    { ghcrrsHealthCheck :: HealthCheck
      -- ^ A complex type that contains the information about the specified health
      -- check.
    } deriving (Eq, Show, Generic)

instance FromXML GetHealthCheckResponse where
    fromXMLOptions = xmlOptions
