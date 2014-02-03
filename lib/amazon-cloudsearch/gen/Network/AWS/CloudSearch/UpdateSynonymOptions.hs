{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.UpdateSynonymOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Configures a synonym dictionary for the search domain. The synonym
-- dictionary is used during indexing to configure mappings for terms that
-- occur in text fields. The maximum size of the synonym dictionary is 100 KB.
module Network.AWS.CloudSearch.UpdateSynonymOptions where

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
updateSynonymOptions :: Text
                     -> Text
                     -> UpdateSynonymOptions
updateSynonymOptions p1 p2 = UpdateSynonymOptions
    { usosDomainName = p1
    , usosSynonyms = p2
    }

data UpdateSynonymOptions = UpdateSynonymOptions
    { usosDomainName :: !Text
      -- ^ A string that represents the name of a domain. Domain names must be unique
      -- across the domains owned by an account within an AWS region. Domain names
      -- must start with a letter or number and can contain the following
      -- characters: a-z (lowercase), 0-9, and - (hyphen). Uppercase letters and
      -- underscores are not allowed.
    , usosSynonyms :: !Text
      -- ^ Maps terms to their synonyms, serialized as a JSON document. The document
      -- has a single object with one property "synonyms" whose value is an object
      -- mapping terms to their synonyms. Each synonym is a simple string or an
      -- array of strings. The maximum size of a stopwords document is 100 KB.
      -- Example: { "synonyms": {"cat": ["feline", "kitten"], "puppy": "dog"} }.
    } deriving (Eq, Show, Generic)

instance ToQuery UpdateSynonymOptions

instance AWSRequest UpdateSynonymOptions where
    type Er UpdateSynonymOptions = CloudSearchError
    type Rs UpdateSynonymOptions = UpdateSynonymOptionsResponse
    request = getQuery service "UpdateSynonymOptions"

data UpdateSynonymOptionsResponse = UpdateSynonymOptionsResponse
    { usosrsSynonyms :: SynonymOptionsStatus
      -- ^ The synonym options configured for this search domain and the current
      -- status of those options.
    } deriving (Eq, Show, Generic)

instance FromXML UpdateSynonymOptionsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "UpdateSynonymOptionsResponse"
        :| ["UpdateSynonymOptionsResult"]
