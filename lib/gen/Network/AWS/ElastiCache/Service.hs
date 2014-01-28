{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.Service
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.ElastiCache.Service where

import Data.Default
import Data.Tagged
import GHC.Generics               (Generic)
import Network.AWS.Internal
import Network.AWS.Internal.Types (Service(..))
import Text.XML.Generic

-- | Currently supported version (@2013-06-15@) of the @Amazon ElastiCache@ service.
service :: Service
service = Service Global v4 "elasticache" "2013-06-15"

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlInherit   = True
    , xmlNamespace = Just "http://elasticache.amazonaws.com/doc/2013-06-15/"
    }

data ElastiCacheError
    = AuthorizationAlreadyExistsFault
    | AuthorizationNotFoundFault
    | CacheClusterAlreadyExistsFault
    | CacheClusterNotFoundFault
    | CacheParameterGroupAlreadyExistsFault
    | CacheParameterGroupNotFoundFault
    | CacheParameterGroupQuotaExceededFault
    | CacheSecurityGroupAlreadyExistsFault
    | CacheSecurityGroupNotFoundFault
    | CacheSecurityGroupQuotaExceededFault
    | CacheSubnetGroupAlreadyExistsFault
    | CacheSubnetGroupInUse
    | CacheSubnetGroupNotFoundFault
    | CacheSubnetGroupQuotaExceededFault
    | CacheSubnetQuotaExceededFault
    | ClusterQuotaForCustomerExceededFault
    | InsufficientCacheClusterCapacityFault
    | InvalidCacheClusterStateFault
    | InvalidCacheParameterGroupStateFault
    | InvalidCacheSecurityGroupStateFault
    | InvalidParameterCombinationException
    | InvalidParameterValueException
    | InvalidReplicationGroupStateFault
    | InvalidSubnet
    | InvalidVPCNetworkStateFault
    | NodeQuotaForClusterExceededFault
    | NodeQuotaForCustomerExceededFault
    | ReplicationGroupAlreadyExistsFault
    | ReplicationGroupNotFoundFault
    | ReservedCacheNodeAlreadyExistsFault
    | ReservedCacheNodeNotFoundFault
    | ReservedCacheNodeQuotaExceededFault
    | ReservedCacheNodesOfferingNotFoundFault
    | SubnetInUse
      deriving (Eq, Show, Generic)

instance FromXML ElastiCacheError where
    fromXMLOptions = xmlOptions
