{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.DescribeDomains
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets information about the search domains owned by this account. Can be
-- limited to specific domains. Shows all domains by default.
module Network.AWS.CloudSearch.DescribeDomains where

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

import Network.AWS.CloudSearch.Service
import Network.AWS.CloudSearch.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeDomains :: DescribeDomains
describeDomains = DescribeDomains
    { ddrDomainNames = []
    }

data DescribeDomains = DescribeDomains
    { ddrDomainNames :: [Text]
      -- ^ Limits the DescribeDomains response to the specified search domains.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeDomains

instance AWSRequest DescribeDomains where
    type Er DescribeDomains = CloudSearchError
    type Rs DescribeDomains = DescribeDomainsResponse
    request = getQuery service "DescribeDomains"

data DescribeDomainsResponse = DescribeDomainsResponse
    { ddrrsDomainStatusList :: [DomainStatus]
      -- ^ The current status of all of your search domains.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeDomainsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeDomainsResponse"
        :| ["DescribeDomainsResult"]
