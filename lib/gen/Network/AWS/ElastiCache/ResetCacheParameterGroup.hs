{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.ResetCacheParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ResetCacheParameterGroup operation modifies the parameters of a cache
-- parameter group to the engine or system default value. You can reset
-- specific parameters by submitting a list of parameter names. To reset the
-- entire cache parameter group, specify the ResetAllParameters and
-- CacheParameterGroupName parameters.
module Network.AWS.ElastiCache.ResetCacheParameterGroup where

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

data ResetCacheParameterGroup = ResetCacheParameterGroup
    { rcpgmCacheParameterGroupName :: !Text
      -- ^ The name of the cache parameter group to reset.
    , rcpgmParameterNameValues :: [ParameterNameValue]
      -- ^ An array of parameter names to be reset. If you are not resetting the
      -- entire cache parameter group, you must specify at least one parameter name.
    , rcpgmResetAllParameters :: Maybe Bool
      -- ^ If true, all parameters in the cache parameter group will be reset to
      -- default values. If false, no such action occurs. Valid values: true |
      -- false.
    } deriving (Eq, Show, Generic)

instance ToQuery ResetCacheParameterGroup

instance AWSRequest ResetCacheParameterGroup where
    type Er ResetCacheParameterGroup = ElastiCacheError
    type Rs ResetCacheParameterGroup = ResetCacheParameterGroupResponse
    request = getQuery service "ResetCacheParameterGroup"

data ResetCacheParameterGroupResponse = ResetCacheParameterGroupResponse
    { rcpgmrsCacheParameterGroupName :: Maybe Text
      -- ^ The name of the cache parameter group.
    } deriving (Eq, Show, Generic)

instance FromXML ResetCacheParameterGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ResetCacheParameterGroupResponse"
        :| ["ResetCacheParameterGroupResult"]
