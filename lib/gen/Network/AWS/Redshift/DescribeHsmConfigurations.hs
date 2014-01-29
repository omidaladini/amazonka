{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DescribeHsmConfigurations
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about the specified Amazon Redshift HSM configuration.
-- If no configuration ID is specified, returns information about all the HSM
-- configurations owned by your AWS customer account.
module Network.AWS.Redshift.DescribeHsmConfigurations where

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

import Network.AWS.Redshift.Service
import Network.AWS.Redshift.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeHsmConfigurations :: DescribeHsmConfigurations
describeHsmConfigurations = DescribeHsmConfigurations
    { dhcnHsmConfigurationIdentifier = Nothing
    , dhcnMarker = Nothing
    , dhcnMaxRecords = Nothing
    }

data DescribeHsmConfigurations = DescribeHsmConfigurations
    { dhcnHsmConfigurationIdentifier :: Maybe Text
      -- ^ The identifier of a specific Amazon Redshift HSM configuration to be
      -- described. If no identifier is specified, information is returned for all
      -- HSM configurations owned by your AWS customer account.
    , dhcnMarker :: Maybe Text
      -- ^ An optional marker returned from a previous DescribeOrderableClusterOptions
      -- request. If this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    , dhcnMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a marker is included in the
      -- response so that the remaining results may be retrieved. Default: 100
      -- Constraints: minimum 20, maximum 100.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeHsmConfigurations

instance AWSRequest DescribeHsmConfigurations where
    type Er DescribeHsmConfigurations = RedshiftError
    type Rs DescribeHsmConfigurations = DescribeHsmConfigurationsResponse
    request = getQuery service "DescribeHsmConfigurations"

data DescribeHsmConfigurationsResponse = DescribeHsmConfigurationsResponse
    { dhcnrsHsmConfigurations :: [HsmConfiguration]
      -- ^ A list of Amazon Redshift HSM configurations.
    , dhcnrsMarker :: Maybe Text
      -- ^ A marker at which to continue listing HSM configurations in a new request.
      -- The response returns a marker if there are more HSM configurations to list
      -- than returned in the response.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeHsmConfigurationsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeHsmConfigurationsResponse"
        :| ["DescribeHsmConfigurationsResult"]
