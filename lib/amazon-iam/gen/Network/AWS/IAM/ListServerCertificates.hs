{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ListServerCertificates
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the server certificates that have the specified path prefix. If none
-- exist, the action returns an empty list. You can paginate the results using
-- the MaxItems and Marker parameters. https://iam.amazonaws.com/
-- ?Action=ListServerCertificates &PathPrefix=/company/servercerts
-- &Version=2010-05-08 &AUTHPARAMS false ProdServerCert /company/servercerts/
-- arn:aws:iam::123456789012:server-certificate/company/servercerts/ProdServerCert
-- 2010-05-08T01:02:03.004Z ASCACKCEVSQ6CEXAMPLE1 BetaServerCert
-- /company/servercerts/
-- arn:aws:iam::123456789012:server-certificate/company/servercerts/BetaServerCert
-- 2010-05-08T02:03:01.004Z ASCACKCEVSQ6CEXAMPLE2 TestServerCert
-- /company/servercerts/
-- arn:aws:iam::123456789012:server-certificate/company/servercerts/TestServerCert
-- 2010-05-08T03:01:02.004Z ASCACKCEVSQ6CEXAMPLE3
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.ListServerCertificates where

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

import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
listServerCertificates :: ListServerCertificates
listServerCertificates = ListServerCertificates
    { lscrMarker = Nothing
    , lscrMaxItems = Nothing
    , lscrPathPrefix = Nothing
    }

data ListServerCertificates = ListServerCertificates
    { lscrMarker :: Maybe Text
      -- ^ Use this only when paginating results, and only in a subsequent request
      -- after you've received a response where the results are truncated. Set it to
      -- the value of the Marker element in the response you just received.
    , lscrMaxItems :: Maybe Int
      -- ^ Use this only when paginating results to indicate the maximum number of
      -- server certificates you want in the response. If there are additional
      -- server certificates beyond the maximum you specify, the IsTruncated
      -- response element will be set to true. This parameter is optional. If you do
      -- not include it, it defaults to 100.
    , lscrPathPrefix :: Maybe Text
      -- ^ The path prefix for filtering the results. For example:
      -- /company/servercerts would get all server certificates for which the path
      -- starts with /company/servercerts. This parameter is optional. If it is not
      -- included, it defaults to a slash (/), listing all server certificates.
    } deriving (Eq, Show, Generic)

instance ToQuery ListServerCertificates

instance AWSRequest ListServerCertificates where
    type Er ListServerCertificates = IAMError
    type Rs ListServerCertificates = ListServerCertificatesResponse
    request = getQuery service "ListServerCertificates"

instance AWSPager ListServerCertificates where
    next rq rs
        | Just x <- lscrrsMarker rs = Just $ rq { lscrMarker = Just x }
        | otherwise = Nothing

data ListServerCertificatesResponse = ListServerCertificatesResponse
    { lscrrsIsTruncated :: Maybe Bool
      -- ^ A flag that indicates whether there are more server certificates to list.
      -- If your results were truncated, you can make a subsequent pagination
      -- request using the Marker request parameter to retrieve more server
      -- certificates in the list.
    , lscrrsMarker :: Maybe Text
      -- ^ If IsTruncated is true, this element is present and contains the value to
      -- use for the Marker parameter in a subsequent pagination request.
    , lscrrsServerCertificateMetadataList :: [ServerCertificateMetadata]
      -- ^ A list of server certificates.
    } deriving (Eq, Show, Generic)

instance FromXML ListServerCertificatesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ListServerCertificatesResponse"
        :| ["ListServerCertificatesResult"]
