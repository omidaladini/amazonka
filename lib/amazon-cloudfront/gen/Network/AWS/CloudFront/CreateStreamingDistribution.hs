{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.CreateStreamingDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Create a new streaming distribution.
module Network.AWS.CloudFront.CreateStreamingDistribution where

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
createStreamingDistribution :: StreamingDistributionConfig
                            -> CreateStreamingDistribution
createStreamingDistribution p1 = CreateStreamingDistribution
    { csdrStreamingDistributionConfig = p1
    }

data CreateStreamingDistribution = CreateStreamingDistribution
    { csdrStreamingDistributionConfig :: StreamingDistributionConfig
      -- ^ The streaming distribution's configuration information.
    } deriving (Eq, Show, Generic)

instance ToHeaders CreateStreamingDistribution

instance ToPath CreateStreamingDistribution where
    toPath = const "/2013-11-11/streaming-distribution"

instance ToQuery CreateStreamingDistribution where
    toQuery = const mempty

instance ToXML CreateStreamingDistribution where
    toXMLOptions = xmlOptions

instance AWSRequest CreateStreamingDistribution where
    type Er CreateStreamingDistribution = CloudFrontError
    type Rs CreateStreamingDistribution = CreateStreamingDistributionResponse
    request = postRestXML service

data CreateStreamingDistributionResponse = CreateStreamingDistributionResponse
    { csdrrsETag :: Maybe Text
      -- ^ The current version of the streaming distribution created.
    , csdrrsLocation :: Maybe Text
      -- ^ The fully qualified URI of the new streaming distribution resource just
      -- created. For example:
      -- https://cloudfront.amazonaws.com/2010-11-01/streaming-distribution/EGTXBD79H29TRA8.
    , csdrrsStreamingDistribution :: Maybe StreamingDistribution
      -- ^ The streaming distribution's information.
    } deriving (Eq, Show, Generic)

instance FromXML CreateStreamingDistributionResponse where
    fromXMLOptions = xmlOptions
