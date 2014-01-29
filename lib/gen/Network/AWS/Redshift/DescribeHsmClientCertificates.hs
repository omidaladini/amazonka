{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DescribeHsmClientCertificates
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about the specified HSM client certificate. If no
-- certificate ID is specified, returns information about all the HSM
-- certificates owned by your AWS customer account.
module Network.AWS.Redshift.DescribeHsmClientCertificates where

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

-- | Convenience method utilising default fields where applicable.
describeHsmClientCertificates :: AWS (Either RedshiftError DescribeHsmClientCertificatesResponse)
describeHsmClientCertificates = undefined $ DescribeHsmClientCertificates
    { dhccmHsmClientCertificateIdentifier = Nothing
    , dhccmMarker = Nothing
    , dhccmMaxRecords = Nothing
    }

data DescribeHsmClientCertificates = DescribeHsmClientCertificates
    { dhccmHsmClientCertificateIdentifier :: Maybe Text
      -- ^ The identifier of a specific HSM client certificate for which you want
      -- information. If no identifier is specified, information is returned for all
      -- HSM client certificates associated with Amazon Redshift clusters owned by
      -- your AWS customer account.
    , dhccmMarker :: Maybe Text
      -- ^ An optional marker returned from a previous DescribeOrderableClusterOptions
      -- request. If this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    , dhccmMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a marker is included in the
      -- response so that the remaining results may be retrieved. Default: 100
      -- Constraints: minimum 20, maximum 100.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeHsmClientCertificates

instance AWSRequest DescribeHsmClientCertificates where
    type Er DescribeHsmClientCertificates = RedshiftError
    type Rs DescribeHsmClientCertificates = DescribeHsmClientCertificatesResponse
    request = getQuery service "DescribeHsmClientCertificates"

data DescribeHsmClientCertificatesResponse = DescribeHsmClientCertificatesResponse
    { dhccmrsHsmClientCertificates :: [HsmClientCertificate]
      -- ^ A list of the identifiers for one or more HSM client certificates used by
      -- Amazon Redshift clusters to store and retrieve database encryption keys in
      -- an HSM.
    , dhccmrsMarker :: Maybe Text
      -- ^ A marker at which to continue listing events in a new request. The response
      -- returns a marker if there are more events to list than returned in the
      -- response.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeHsmClientCertificatesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeHsmClientCertificatesResponse"
        :| ["DescribeHsmClientCertificatesResult"]
