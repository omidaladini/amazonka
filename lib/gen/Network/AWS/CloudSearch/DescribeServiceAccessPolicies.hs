{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.DescribeServiceAccessPolicies
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets information about the resource-based policies that control access to
-- the domain's document and search services.
module Network.AWS.CloudSearch.DescribeServiceAccessPolicies where

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

data DescribeServiceAccessPolicies = DescribeServiceAccessPolicies
    { dsaprDomainName :: !Text
      -- ^ A string that represents the name of a domain. Domain names must be unique
      -- across the domains owned by an account within an AWS region. Domain names
      -- must start with a letter or number and can contain the following
      -- characters: a-z (lowercase), 0-9, and - (hyphen). Uppercase letters and
      -- underscores are not allowed.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeServiceAccessPolicies

instance AWSRequest DescribeServiceAccessPolicies where
    type Er DescribeServiceAccessPolicies = CloudSearchError
    type Rs DescribeServiceAccessPolicies = DescribeServiceAccessPoliciesResponse
    request = getQuery service "DescribeServiceAccessPolicies"

data DescribeServiceAccessPoliciesResponse = DescribeServiceAccessPoliciesResponse
    { dsaprrsAccessPolicies :: AccessPoliciesStatus
      -- ^ A PolicyDocument that specifies access policies for the search domain's
      -- services, and the current status of those policies.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeServiceAccessPoliciesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeServiceAccessPoliciesResponse"
        :| ["DescribeServiceAccessPoliciesResult"]
