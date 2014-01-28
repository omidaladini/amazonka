{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.DefineRankExpression
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Configures a RankExpression for the search domain. Used to create new rank
-- expressions and modify existing ones. If the expression exists, the new
-- configuration replaces the old one. You can configure a maximum of 50 rank
-- expressions.
module Network.AWS.CloudSearch.DefineRankExpression where

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

data DefineRankExpression = DefineRankExpression
    { dretDomainName :: !Text
      -- ^ A string that represents the name of a domain. Domain names must be unique
      -- across the domains owned by an account within an AWS region. Domain names
      -- must start with a letter or number and can contain the following
      -- characters: a-z (lowercase), 0-9, and - (hyphen). Uppercase letters and
      -- underscores are not allowed.
    , dretRankExpression :: NamedRankExpression
      -- ^ A named expression that can be evaluated at search time and used for
      -- ranking or thresholding in a search query.
    } deriving (Eq, Show, Generic)

instance ToQuery DefineRankExpression

instance AWSRequest DefineRankExpression where
    type Er DefineRankExpression = CloudSearchError
    type Rs DefineRankExpression = DefineRankExpressionResponse
    request = getQuery service "DefineRankExpression"

data DefineRankExpressionResponse = DefineRankExpressionResponse
    { dretrsRankExpression :: RankExpressionStatus
      -- ^ The value of a RankExpression and its current status.
    } deriving (Eq, Show, Generic)

instance FromXML DefineRankExpressionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DefineRankExpressionResponse"
        :| ["DefineRankExpressionResult"]
