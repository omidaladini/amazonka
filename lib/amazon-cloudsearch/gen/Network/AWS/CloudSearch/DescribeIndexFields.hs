{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.DescribeIndexFields
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets information about the index fields configured for the search domain.
-- Can be limited to specific fields by name. Shows all fields by default.
module Network.AWS.CloudSearch.DescribeIndexFields where

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
describeIndexFields :: Text
                    -> DescribeIndexFields
describeIndexFields p1 = DescribeIndexFields
    { difrDomainName = p1
    , difrFieldNames = []
    }

data DescribeIndexFields = DescribeIndexFields
    { difrDomainName :: !Text
      -- ^ A string that represents the name of a domain. Domain names must be unique
      -- across the domains owned by an account within an AWS region. Domain names
      -- must start with a letter or number and can contain the following
      -- characters: a-z (lowercase), 0-9, and - (hyphen). Uppercase letters and
      -- underscores are not allowed.
    , difrFieldNames :: [Text]
      -- ^ Limits the DescribeIndexFields response to the specified fields.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeIndexFields

instance AWSRequest DescribeIndexFields where
    type Er DescribeIndexFields = CloudSearchError
    type Rs DescribeIndexFields = DescribeIndexFieldsResponse
    request = getQuery service "DescribeIndexFields"

data DescribeIndexFieldsResponse = DescribeIndexFieldsResponse
    { difrrsIndexFields :: [IndexFieldStatus]
      -- ^ The index fields configured for the domain.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeIndexFieldsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeIndexFieldsResponse"
        :| ["DescribeIndexFieldsResult"]
