{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DescribeDefaultClusterParameters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of parameter settings for the specified parameter group
-- family. For more information about managing parameter groups, go to Amazon
-- Redshift Parameter Groups in the Amazon Redshift Management Guide.
-- https://redshift.us-east-1.amazonaws.com/
-- ?Action=DescribeDefaultClusterParameters &ParameterGroupFamily=redshift-1.0
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121207/us-east-1/redshift/aws4_request
-- &x-amz-date=20121207T231708Z
-- &x-amz-signedheaders=content-type;host;x-amz-date redshift-1.0 ISO, MDY
-- string engine-default true Sets the display format for date and time
-- values. datestyle 0 integer engine-default true Sets the number of digits
-- displayed for floating-point values -15-2 extra_float_digits default string
-- engine-default true This parameter applies a user-defined label to a group
-- of queries that are run during the same session.. query_group false boolean
-- engine-default true require ssl for all databaseconnections true,false
-- require_ssl $user, public string engine-default true Sets the schema search
-- order for names that are not schema-qualified. search_path 0 integer
-- engine-default true Aborts any statement that takes over the specified
-- number of milliseconds. statement_timeout
-- [{&quot;query_concurrency&quot;:5}] string engine-default true wlm json
-- configuration wlm_json_configuration 396df00b-40c4-11e2-82cf-0b45b05c0221.
module Network.AWS.Redshift.DescribeDefaultClusterParameters where

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
describeDefaultClusterParameters :: Text
                                 -> DescribeDefaultClusterParameters
describeDefaultClusterParameters p1 = DescribeDefaultClusterParameters
    { ddcpmParameterGroupFamily = p1
    , ddcpmMarker = Nothing
    , ddcpmMaxRecords = Nothing
    }

data DescribeDefaultClusterParameters = DescribeDefaultClusterParameters
    { ddcpmMarker :: Maybe Text
      -- ^ An optional marker returned from a previous
      -- DescribeDefaultClusterParameters request. If this parameter is specified,
      -- the response includes only records beyond the marker, up to the value
      -- specified by MaxRecords.
    , ddcpmMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a marker is included in the
      -- response so that the remaining results may be retrieved. Default: 100
      -- Constraints: Value must be at least 20 and no more than 100.
    , ddcpmParameterGroupFamily :: !Text
      -- ^ The name of the cluster parameter group family.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeDefaultClusterParameters

instance AWSRequest DescribeDefaultClusterParameters where
    type Er DescribeDefaultClusterParameters = RedshiftError
    type Rs DescribeDefaultClusterParameters = DescribeDefaultClusterParametersResponse
    request = getQuery service "DescribeDefaultClusterParameters"

data DescribeDefaultClusterParametersResponse = DescribeDefaultClusterParametersResponse
    { ddcpmrsDefaultClusterParameters :: Maybe DefaultClusterParameters
      -- ^ Describes the default cluster parameters for a parameter group family.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeDefaultClusterParametersResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeDefaultClusterParametersResponse"
        :| ["DescribeDefaultClusterParametersResult"]
