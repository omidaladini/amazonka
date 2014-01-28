{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.DefineIndexField
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Configures an IndexField for the search domain. Used to create new fields
-- and modify existing ones. If the field exists, the new configuration
-- replaces the old one. You can configure a maximum of 200 index fields.
module Network.AWS.CloudSearch.DefineIndexField where

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

data DefineIndexField = DefineIndexField
    { diftDomainName :: !Text
      -- ^ A string that represents the name of a domain. Domain names must be unique
      -- across the domains owned by an account within an AWS region. Domain names
      -- must start with a letter or number and can contain the following
      -- characters: a-z (lowercase), 0-9, and - (hyphen). Uppercase letters and
      -- underscores are not allowed.
    , diftIndexField :: IndexField
      -- ^ Defines a field in the index, including its name, type, and the source of
      -- its data. The IndexFieldType indicates which of the options will be
      -- present. It is invalid to specify options for a type other than the
      -- IndexFieldType.
    } deriving (Eq, Show, Generic)

instance ToQuery DefineIndexField

instance AWSRequest DefineIndexField where
    type Er DefineIndexField = CloudSearchError
    type Rs DefineIndexField = DefineIndexFieldResponse
    request = getQuery service "DefineIndexField"

data DefineIndexFieldResponse = DefineIndexFieldResponse
    { diftrsIndexField :: IndexFieldStatus
      -- ^ The value of an IndexField and its current status.
    } deriving (Eq, Show, Generic)

instance FromXML DefineIndexFieldResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DefineIndexFieldResponse"
        :| ["DefineIndexFieldResult"]