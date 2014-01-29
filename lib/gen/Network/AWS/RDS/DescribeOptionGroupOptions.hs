{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DescribeOptionGroupOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes all available options. https://rds.amazonaws.com/
-- ?Action=DescribeOptionGroupOptions &EngineName=oracle-se1
-- &MajorEngineVersion=11.2 11.2 true Oracle Enterprise Manager 1158 OEM
-- oracle-se1 0.2.v3 false false d9c8f6a1-84c7-11e1-a264-0b23c28bc344.
module Network.AWS.RDS.DescribeOptionGroupOptions where

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

import Network.AWS.RDS.Service
import Network.AWS.RDS.Types

-- | Convenience method utilising default fields where applicable.
describeOptionGroupOptions :: Text
                           -> AWS (Either RDSError DescribeOptionGroupOptionsResponse)
describeOptionGroupOptions p1 = undefined $ DescribeOptionGroupOptions
    { dogomEngineName = p1
    , dogomMajorEngineVersion = Nothing
    , dogomMarker = Nothing
    , dogomMaxRecords = Nothing
    }

data DescribeOptionGroupOptions = DescribeOptionGroupOptions
    { dogomEngineName :: !Text
      -- ^ A required parameter. Options available for the given Engine name will be
      -- described.
    , dogomMajorEngineVersion :: Maybe Text
      -- ^ If specified, filters the results to include only options for the specified
      -- major engine version.
    , dogomMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If this
      -- parameter is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    , dogomMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a pagination token called a
      -- marker is included in the response so that the remaining results can be
      -- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeOptionGroupOptions

instance AWSRequest DescribeOptionGroupOptions where
    type Er DescribeOptionGroupOptions = RDSError
    type Rs DescribeOptionGroupOptions = DescribeOptionGroupOptionsResponse
    request = getQuery service "DescribeOptionGroupOptions"

instance AWSPager DescribeOptionGroupOptions where
    next rq rs
        | Just x <- dogomrsMarker rs = Just $ rq { dogomMarker = Just x }
        | otherwise = Nothing

data DescribeOptionGroupOptionsResponse = DescribeOptionGroupOptionsResponse
    { dogomrsMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If this
      -- parameter is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    , dogomrsOptionGroupOptions :: [OptionGroupOption]
      -- ^ List of available option group options.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeOptionGroupOptionsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeOptionGroupOptionsResponse"
        :| ["DescribeOptionGroupOptionsResult"]
