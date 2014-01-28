{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.DescribeRankExpressions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets the rank expressions configured for the search domain. Can be limited
-- to specific rank expressions by name. Shows all rank expressions by
-- default.
module Network.AWS.CloudSearch.DescribeRankExpressions where

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

data DescribeRankExpressions = DescribeRankExpressions
    { drerDomainName :: !Text
      -- ^ A string that represents the name of a domain. Domain names must be unique
      -- across the domains owned by an account within an AWS region. Domain names
      -- must start with a letter or number and can contain the following
      -- characters: a-z (lowercase), 0-9, and - (hyphen). Uppercase letters and
      -- underscores are not allowed.
    , drerRankNames :: [Text]
      -- ^ Limits the DescribeRankExpressions response to the specified fields.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeRankExpressions

instance AWSRequest DescribeRankExpressions where
    type Er DescribeRankExpressions = CloudSearchError
    type Rs DescribeRankExpressions = DescribeRankExpressionsResponse
    request = getQuery service "DescribeRankExpressions"

data DescribeRankExpressionsResponse = DescribeRankExpressionsResponse
    { drerrsRankExpressions :: [RankExpressionStatus]
      -- ^ The rank expressions configured for the domain.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeRankExpressionsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeRankExpressionsResponse"
        :| ["DescribeRankExpressionsResult"]
