{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.ModifyCacheParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ModifyCacheParameterGroup operation modifies the parameters of a cache
-- parameter group. You can modify up to 20 parameters in a single request by
-- submitting a list parameter name and value pairs.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=ModifyCacheParameterGroup
-- ?ParameterNameValues.member.1.ParameterName=chunk_size_growth_factor
-- &ParameterNameValues.member.1.ParameterValue=1.02
-- &CacheParameterGroupName=mycacheparametergroup &Version=2013-06-15
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-07-27T03%3A24%3A50.203Z &AWSAccessKeyId=YOUR-ACCESS-KEY
-- &Signature=YOUR-SIGNATURE mycacheparametergroup
-- fcedeef2-b7ff-11e0-9326-b7275b9d4a6c.
module Network.AWS.ElastiCache.ModifyCacheParameterGroup where

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

import Network.AWS.ElastiCache.Service
import Network.AWS.ElastiCache.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
modifyCacheParameterGroup :: Text
                          -> [ParameterNameValue]
                          -> ModifyCacheParameterGroup
modifyCacheParameterGroup p1 p2 = ModifyCacheParameterGroup
    { mcpgmCacheParameterGroupName = p1
    , mcpgmParameterNameValues = p2
    }

data ModifyCacheParameterGroup = ModifyCacheParameterGroup
    { mcpgmCacheParameterGroupName :: !Text
      -- ^ The name of the cache parameter group to modify.
    , mcpgmParameterNameValues :: [ParameterNameValue]
      -- ^ An array of parameter names and values for the parameter update. You must
      -- supply at least one parameter name and value; subsequent arguments are
      -- optional. A maximum of 20 parameters may be modified per request.
    } deriving (Eq, Show, Generic)

instance ToQuery ModifyCacheParameterGroup

instance AWSRequest ModifyCacheParameterGroup where
    type Er ModifyCacheParameterGroup = ElastiCacheError
    type Rs ModifyCacheParameterGroup = ModifyCacheParameterGroupResponse
    request = getQuery service "ModifyCacheParameterGroup"

data ModifyCacheParameterGroupResponse = ModifyCacheParameterGroupResponse
    { mcpgmrsCacheParameterGroupName :: Maybe Text
      -- ^ The name of the cache parameter group.
    } deriving (Eq, Show, Generic)

instance FromXML ModifyCacheParameterGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ModifyCacheParameterGroupResponse"
        :| ["ModifyCacheParameterGroupResult"]
