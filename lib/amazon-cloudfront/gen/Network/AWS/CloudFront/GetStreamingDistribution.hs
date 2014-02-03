{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.GetStreamingDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Get the information about a streaming distribution.
module Network.AWS.CloudFront.GetStreamingDistribution where

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
getStreamingDistribution :: Text
                         -> GetStreamingDistribution
getStreamingDistribution p1 = GetStreamingDistribution
    { gsdrId = p1
    }

data GetStreamingDistribution = GetStreamingDistribution
    { gsdrId :: !Text
      -- ^ The streaming distribution's id.
    } deriving (Eq, Show, Generic)

instance ToHeaders GetStreamingDistribution

instance ToPath GetStreamingDistribution where
    toPath GetStreamingDistribution{..} = Text.concat
        [ "/2013-11-11/streaming-distribution/"
        , toText gsdrId
        ]

instance ToQuery GetStreamingDistribution where
    toQuery = const mempty

instance ToXML GetStreamingDistribution where
    toXMLOptions = xmlOptions

instance AWSRequest GetStreamingDistribution where
    type Er GetStreamingDistribution = CloudFrontError
    type Rs GetStreamingDistribution = GetStreamingDistributionResponse
    request = getRestXML service

data GetStreamingDistributionResponse = GetStreamingDistributionResponse
    { gsdrrsETag :: Maybe Text
      -- ^ The current version of the streaming distribution's information. For
      -- example: E2QWRUHAPOMQZL.
    , gsdrrsStreamingDistribution :: Maybe StreamingDistribution
      -- ^ The streaming distribution's information.
    } deriving (Eq, Show, Generic)

instance FromXML GetStreamingDistributionResponse where
    fromXMLOptions = xmlOptions
