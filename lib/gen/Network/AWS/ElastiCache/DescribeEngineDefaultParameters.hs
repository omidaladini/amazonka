{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.DescribeEngineDefaultParameters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeEngineDefaultParameters operation returns the default engine
-- and system parameter information for the specified cache engine. Some of
-- the output has been omitted for brevity.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=DescribeEngineDefaultParameters
-- &CacheParameterGroupFamily=memcached1.4 &MaxRecords=100 &Version=2013-06-15
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-07-27T01%3A34%3A31.045Z &AWSAccessKeyId=YOUR-ACCESS-KEY
-- &Signature=YOUR-SIGNATURE memcached1.4 1024 integer system false The
-- backlog queue limit. 1-10000 backlog_queue_limit 1.4.5 (...output
-- omitted...) cache.c1.xlarge 6000 (...output omitted...) integer system
-- false The maximum configurable amount of memory to use to store items, in
-- megabytes. 1-100000 max_cache_memory 1.4.5 (...output omitted...)
-- 061282fe-b7fd-11e0-9326-b7275b9d4a6c.
module Network.AWS.ElastiCache.DescribeEngineDefaultParameters where

import           Control.Monad
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import           Text.XML.Generic

import Network.AWS.ElastiCache.Service
import Network.AWS.ElastiCache.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeEngineDefaultParameters :: Text
                                -> DescribeEngineDefaultParameters
describeEngineDefaultParameters p1 = undefined $ DescribeEngineDefaultParameters
    { dedpmCacheParameterGroupFamily = p1
    , dedpmMarker = Nothing
    , dedpmMaxRecords = Nothing
    }

data DescribeEngineDefaultParameters = DescribeEngineDefaultParameters
    { dedpmCacheParameterGroupFamily :: !Text
      -- ^ The name of the cache parameter group family. Valid values are:
      -- memcached1.4 | redis2.6.
    , dedpmMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker for
      -- pagination of results from this operation. If this parameter is specified,
      -- the response includes only records beyond the marker, up to the value
      -- specified by MaxRecords.
    , dedpmMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a marker is included in the
      -- response so that the remaining results can be retrieved. Default:
      -- 100Constraints: minimum 20; maximum 100.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeEngineDefaultParameters

instance AWSRequest DescribeEngineDefaultParameters where
    type Er DescribeEngineDefaultParameters = ElastiCacheError
    type Rs DescribeEngineDefaultParameters = DescribeEngineDefaultParametersResponse
    request = getQuery service "DescribeEngineDefaultParameters"

instance AWSPager DescribeEngineDefaultParameters where
    next rq rs
        | Just _ <- marker = Just $ rq { dedpmMarker = marker }
        | otherwise        = Nothing
      where
        marker = join . fmap edMarker $ dedpmrsEngineDefaults rs

data DescribeEngineDefaultParametersResponse = DescribeEngineDefaultParametersResponse
    { dedpmrsEngineDefaults :: Maybe EngineDefaults
      -- ^ Represents the output of a DescribeEngineDefaultParameters operation.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeEngineDefaultParametersResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeEngineDefaultParametersResponse"
        :| ["DescribeEngineDefaultParametersResult"]
