{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.DescribeDefaultSearchField
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets the default search field configured for the search domain.
module Network.AWS.CloudSearch.DescribeDefaultSearchField where

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
describeDefaultSearchField :: Text
                           -> DescribeDefaultSearchField
describeDefaultSearchField p1 = undefined $ DescribeDefaultSearchField
    { ddsfrDomainName = p1
    }

data DescribeDefaultSearchField = DescribeDefaultSearchField
    { ddsfrDomainName :: !Text
      -- ^ A string that represents the name of a domain. Domain names must be unique
      -- across the domains owned by an account within an AWS region. Domain names
      -- must start with a letter or number and can contain the following
      -- characters: a-z (lowercase), 0-9, and - (hyphen). Uppercase letters and
      -- underscores are not allowed.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeDefaultSearchField

instance AWSRequest DescribeDefaultSearchField where
    type Er DescribeDefaultSearchField = CloudSearchError
    type Rs DescribeDefaultSearchField = DescribeDefaultSearchFieldResponse
    request = getQuery service "DescribeDefaultSearchField"

data DescribeDefaultSearchFieldResponse = DescribeDefaultSearchFieldResponse
    { ddsfrrsDefaultSearchField :: DefaultSearchFieldStatus
      -- ^ The name of the IndexField to use for search requests issued with the q
      -- parameter. The default is the empty string, which automatically searches
      -- all text fields.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeDefaultSearchFieldResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeDefaultSearchFieldResponse"
        :| ["DescribeDefaultSearchFieldResult"]
