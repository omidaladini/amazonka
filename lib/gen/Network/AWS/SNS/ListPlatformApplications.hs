{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.ListPlatformApplications
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ListPlatformApplications action lists the platform application objects
-- for the supported push notification services, such as APNS and GCM. The
-- results for ListPlatformApplications are paginated and return a limited
-- list of applications, up to 100. If additional records are available after
-- the first page results, then a NextToken string will be returned. To
-- receive the next page, you call ListPlatformApplications using the
-- NextToken string received from the previous call. When there are no more
-- records to return, NextToken will be null. For more information, see Using
-- Amazon SNS Mobile Push Notifications. POST
-- http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ...
-- Action=ListPlatformApplications &SignatureMethod=HmacSHA256
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &SignatureVersion=2
-- &Version=2010-03-31
-- &Signature=drVbTuyR5N9e88WJMNPzBOjNFNvawkCaMfZI0xa9kIQ%3D
-- &Timestamp=2013-07-01T22%3A33%3A55.618Z HTTP/1.1 200 OK ...
-- arn:aws:sns:us-west-2:123456789012:app/APNS_SANDBOX/apnspushapp
-- AllowEndpointPolicies false
-- arn:aws:sns:us-west-2:123456789012:app/GCM/gcmpushapp AllowEndpointPolicies
-- false 315a335e-85d8-52df-9349-791283cbb529.
module Network.AWS.SNS.ListPlatformApplications where

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

import Network.AWS.SNS.Service
import Network.AWS.SNS.Types

data ListPlatformApplications = ListPlatformApplications
    { lpaiNextToken :: Maybe Text
      -- ^ NextToken string is used when calling ListPlatformApplications action to
      -- retrieve additional records that are available after the first page
      -- results.
    } deriving (Eq, Show, Generic)

instance ToQuery ListPlatformApplications

instance AWSRequest ListPlatformApplications where
    type Er ListPlatformApplications = SNSError
    type Rs ListPlatformApplications = ListPlatformApplicationsResponse
    request = getQuery service "ListPlatformApplications"

data ListPlatformApplicationsResponse = ListPlatformApplicationsResponse
    { lpairsNextToken :: Maybe Text
      -- ^ NextToken string is returned when calling ListPlatformApplications action
      -- if additional records are available after the first page results.
    , lpairsPlatformApplications :: [PlatformApplication]
      -- ^ Platform applications returned when calling ListPlatformApplications
      -- action.
    } deriving (Eq, Show, Generic)

instance FromXML ListPlatformApplicationsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ListPlatformApplicationsResponse"
        :| ["ListPlatformApplicationsResult"]
