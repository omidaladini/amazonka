{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DescribeClusterParameters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a detailed list of parameters contained within the specified Amazon
-- Redshift parameter group. For each parameter the response includes
-- information such as parameter name, description, data type, value, whether
-- the parameter value is modifiable, and so on. You can specify source filter
-- to retrieve parameters of only specific type. For example, to retrieve
-- parameters that were modified by a user action such as from
-- ModifyClusterParameterGroup, you can specify source equal to user. For more
-- information about managing parameter groups, go to Amazon Redshift
-- Parameter Groups in the Amazon Redshift Management Guide.
-- https://redshift.us-east-1.amazonaws.com/ ?Action=DescribeClusterParameters
-- &ParameterGroupName=parametergroup1 &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121208/us-east-1/redshift/aws4_request
-- &x-amz-date=20121208T010408Z
-- &x-amz-signedheaders=content-type;host;x-amz-date ISO, MDY string
-- engine-default true Sets the display format for date and time values.
-- datestyle 0 integer engine-default true Sets the number of digits displayed
-- for floating-point values -15-2 extra_float_digits default string
-- engine-default true This parameter applies a user-defined label to a group
-- of queries that are run during the same session.. query_group false boolean
-- engine-default true require ssl for all databaseconnections true,false
-- require_ssl $user, public string engine-default true Sets the schema search
-- order for names that are not schema-qualified. search_path 0 integer
-- engine-default true Aborts any statement that takes over the specified
-- number of milliseconds. statement_timeout
-- [{&quot;query_concurrency&quot;:5}] string engine-default true wlm json
-- configuration wlm_json_configuration 2ba35df4-40d3-11e2-82cf-0b45b05c0221.
module Network.AWS.Redshift.DescribeClusterParameters where

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
describeClusterParameters :: Text
                          -> DescribeClusterParameters
describeClusterParameters p1 = DescribeClusterParameters
    { dcpmParameterGroupName = p1
    , dcpmMarker = Nothing
    , dcpmMaxRecords = Nothing
    , dcpmSource = Nothing
    }

data DescribeClusterParameters = DescribeClusterParameters
    { dcpmMarker :: Maybe Text
      -- ^ An optional marker returned from a previous DescribeClusterParameters
      -- request. If this parameter is specified, the response includes only records
      -- beyond the specified marker, up to the value specified by MaxRecords.
    , dcpmMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, response includes a marker that
      -- you can specify in your subsequent request to retrieve remaining result.
      -- Default: 100 Constraints: Value must be at least 20 and no more than 100.
    , dcpmParameterGroupName :: !Text
      -- ^ The name of a cluster parameter group for which to return details.
    , dcpmSource :: Maybe Text
      -- ^ The parameter types to return. Specify user to show parameters that are
      -- different form the default. Similarly, specify engine-default to show
      -- parameters that are the same as the default parameter group. Default: All
      -- parameter types returned. Valid Values: user | engine-default.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeClusterParameters

instance AWSRequest DescribeClusterParameters where
    type Er DescribeClusterParameters = RedshiftError
    type Rs DescribeClusterParameters = DescribeClusterParametersResponse
    request = getQuery service "DescribeClusterParameters"

data DescribeClusterParametersResponse = DescribeClusterParametersResponse
    { dcpmrsMarker :: Maybe Text
      -- ^ A marker that indicates the first parameter group that a subsequent
      -- DescribeClusterParameterGroups request will return. The response returns a
      -- marker only if there are more parameter groups details to list than the
      -- current response can return.
    , dcpmrsParameters :: [Parameter]
      -- ^ A list of Parameter instances. Each instance lists the parameters of one
      -- cluster parameter group.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeClusterParametersResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeClusterParametersResponse"
        :| ["DescribeClusterParametersResult"]
