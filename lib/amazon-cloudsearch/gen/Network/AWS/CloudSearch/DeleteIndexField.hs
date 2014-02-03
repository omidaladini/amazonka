{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.DeleteIndexField
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes an IndexField from the search domain.
module Network.AWS.CloudSearch.DeleteIndexField where

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
deleteIndexField :: Text
                 -> Text
                 -> DeleteIndexField
deleteIndexField p1 p2 = DeleteIndexField
    { difsDomainName = p1
    , difsIndexFieldName = p2
    }

data DeleteIndexField = DeleteIndexField
    { difsDomainName :: !Text
      -- ^ A string that represents the name of a domain. Domain names must be unique
      -- across the domains owned by an account within an AWS region. Domain names
      -- must start with a letter or number and can contain the following
      -- characters: a-z (lowercase), 0-9, and - (hyphen). Uppercase letters and
      -- underscores are not allowed.
    , difsIndexFieldName :: !Text
      -- ^ A string that represents the name of an index field. Field names must begin
      -- with a letter and can contain the following characters: a-z (lowercase),
      -- 0-9, and _ (underscore). Uppercase letters and hyphens are not allowed. The
      -- names "body", "docid", and "text_relevance" are reserved and cannot be
      -- specified as field or rank expression names.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteIndexField

instance AWSRequest DeleteIndexField where
    type Er DeleteIndexField = CloudSearchError
    type Rs DeleteIndexField = DeleteIndexFieldResponse
    request = getQuery service "DeleteIndexField"

data DeleteIndexFieldResponse = DeleteIndexFieldResponse
    { difsrsIndexField :: IndexFieldStatus
      -- ^ The value of an IndexField and its current status.
    } deriving (Eq, Show, Generic)

instance FromXML DeleteIndexFieldResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeleteIndexFieldResponse"
        :| ["DeleteIndexFieldResult"]
