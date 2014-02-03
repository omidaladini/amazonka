{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.UpdateStemmingOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Configures a stemming dictionary for the search domain. The stemming
-- dictionary is used during indexing and when processing search requests. The
-- maximum size of the stemming dictionary is 500 KB.
module Network.AWS.CloudSearch.UpdateStemmingOptions where

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
updateStemmingOptions :: Text
                      -> Text
                      -> UpdateStemmingOptions
updateStemmingOptions p1 p2 = UpdateStemmingOptions
    { usotDomainName = p1
    , usotStems = p2
    }

data UpdateStemmingOptions = UpdateStemmingOptions
    { usotDomainName :: !Text
      -- ^ A string that represents the name of a domain. Domain names must be unique
      -- across the domains owned by an account within an AWS region. Domain names
      -- must start with a letter or number and can contain the following
      -- characters: a-z (lowercase), 0-9, and - (hyphen). Uppercase letters and
      -- underscores are not allowed.
    , usotStems :: !Text
      -- ^ Maps terms to their stems, serialized as a JSON document. The document has
      -- a single object with one property "stems" whose value is an object mapping
      -- terms to their stems. The maximum size of a stemming document is 500 KB.
      -- Example: { "stems": {"people": "person", "walking": "walk"} }.
    } deriving (Eq, Show, Generic)

instance ToQuery UpdateStemmingOptions

instance AWSRequest UpdateStemmingOptions where
    type Er UpdateStemmingOptions = CloudSearchError
    type Rs UpdateStemmingOptions = UpdateStemmingOptionsResponse
    request = getQuery service "UpdateStemmingOptions"

data UpdateStemmingOptionsResponse = UpdateStemmingOptionsResponse
    { usotrsStems :: StemmingOptionsStatus
      -- ^ The stemming options configured for this search domain and the current
      -- status of those options.
    } deriving (Eq, Show, Generic)

instance FromXML UpdateStemmingOptionsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "UpdateStemmingOptionsResponse"
        :| ["UpdateStemmingOptionsResult"]
