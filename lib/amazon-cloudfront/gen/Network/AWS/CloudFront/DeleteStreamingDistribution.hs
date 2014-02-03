{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.DeleteStreamingDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Delete a streaming distribution.
module Network.AWS.CloudFront.DeleteStreamingDistribution where

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
deleteStreamingDistribution :: Text
                            -> Text
                            -> DeleteStreamingDistribution
deleteStreamingDistribution p1 p2 = DeleteStreamingDistribution
    { dsdrId = p1
    , dsdrIfMatch = p2
    }

data DeleteStreamingDistribution = DeleteStreamingDistribution
    { dsdrId :: !Text
      -- ^ The distribution id.
    , dsdrIfMatch :: !Text
      -- ^ The value of the ETag header you received when you disabled the streaming
      -- distribution. For example: E2QWRUHAPOMQZL.
    } deriving (Eq, Show, Generic)

instance ToHeaders DeleteStreamingDistribution where
    toHeaders DeleteStreamingDistribution{..} =
        [ "If-Match" =: dsdrIfMatch
        ]

instance ToPath DeleteStreamingDistribution where
    toPath DeleteStreamingDistribution{..} = Text.concat
        [ "/2013-11-11/streaming-distribution/"
        , toText dsdrId
        ]

instance ToQuery DeleteStreamingDistribution where
    toQuery = const mempty

instance ToXML DeleteStreamingDistribution where
    toXMLOptions = xmlOptions

instance AWSRequest DeleteStreamingDistribution where
    type Er DeleteStreamingDistribution = CloudFrontError
    type Rs DeleteStreamingDistribution = DeleteStreamingDistributionResponse
    request = deleteRestXML service

data DeleteStreamingDistributionResponse = DeleteStreamingDistributionResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteStreamingDistributionResponse where
    fromXMLOptions = xmlOptions
