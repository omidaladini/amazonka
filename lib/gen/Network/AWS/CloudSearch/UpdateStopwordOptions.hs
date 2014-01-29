{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.UpdateStopwordOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Configures stopwords for the search domain. Stopwords are used during
-- indexing and when processing search requests. The maximum size of the
-- stopwords dictionary is 10 KB.
module Network.AWS.CloudSearch.UpdateStopwordOptions where

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

-- | Convenience method utilising default fields where applicable.
updateStopwordOptions :: Text
                      -> Text
                      -> AWS (Either CloudSearchError UpdateStopwordOptionsResponse)
updateStopwordOptions p1 p2 = undefined $ UpdateStopwordOptions
    { usorDomainName = p1
    , usorStopwords = p2
    }

data UpdateStopwordOptions = UpdateStopwordOptions
    { usorDomainName :: !Text
      -- ^ A string that represents the name of a domain. Domain names must be unique
      -- across the domains owned by an account within an AWS region. Domain names
      -- must start with a letter or number and can contain the following
      -- characters: a-z (lowercase), 0-9, and - (hyphen). Uppercase letters and
      -- underscores are not allowed.
    , usorStopwords :: !Text
      -- ^ Lists stopwords serialized as a JSON document. The document has a single
      -- object with one property "stopwords" whose value is an array of strings.
      -- The maximum size of a stopwords document is 10 KB. Example: { "stopwords":
      -- ["a", "an", "the", "of"] }.
    } deriving (Eq, Show, Generic)

instance ToQuery UpdateStopwordOptions

instance AWSRequest UpdateStopwordOptions where
    type Er UpdateStopwordOptions = CloudSearchError
    type Rs UpdateStopwordOptions = UpdateStopwordOptionsResponse
    request = getQuery service "UpdateStopwordOptions"

data UpdateStopwordOptionsResponse = UpdateStopwordOptionsResponse
    { usorrsStopwords :: StopwordOptionsStatus
      -- ^ The stopword options configured for this search domain and the current
      -- status of those options.
    } deriving (Eq, Show, Generic)

instance FromXML UpdateStopwordOptionsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "UpdateStopwordOptionsResponse"
        :| ["UpdateStopwordOptionsResult"]
