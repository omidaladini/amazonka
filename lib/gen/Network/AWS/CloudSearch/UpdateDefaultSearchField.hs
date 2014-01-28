{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.UpdateDefaultSearchField
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Configures the default search field for the search domain. The default
-- search field is used when a search request does not specify which fields to
-- search. By default, it is configured to include the contents of all of the
-- domain's text fields.
module Network.AWS.CloudSearch.UpdateDefaultSearchField where

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

data UpdateDefaultSearchField = UpdateDefaultSearchField
    { udsfrDefaultSearchField :: !Text
      -- ^ The IndexField to use for search requests issued with the q parameter. The
      -- default is an empty string, which automatically searches all text fields.
    , udsfrDomainName :: !Text
      -- ^ A string that represents the name of a domain. Domain names must be unique
      -- across the domains owned by an account within an AWS region. Domain names
      -- must start with a letter or number and can contain the following
      -- characters: a-z (lowercase), 0-9, and - (hyphen). Uppercase letters and
      -- underscores are not allowed.
    } deriving (Eq, Show, Generic)

instance ToQuery UpdateDefaultSearchField

instance AWSRequest UpdateDefaultSearchField where
    type Er UpdateDefaultSearchField = CloudSearchError
    type Rs UpdateDefaultSearchField = UpdateDefaultSearchFieldResponse
    request = getQuery service "UpdateDefaultSearchField"

data UpdateDefaultSearchFieldResponse = UpdateDefaultSearchFieldResponse
    { udsfrrsDefaultSearchField :: DefaultSearchFieldStatus
      -- ^ The value of the DefaultSearchField configured for this search domain and
      -- its current status.
    } deriving (Eq, Show, Generic)

instance FromXML UpdateDefaultSearchFieldResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "UpdateDefaultSearchFieldResponse"
        :| ["UpdateDefaultSearchFieldResult"]
