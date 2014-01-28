{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.CreateCacheSecurityGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateCacheSecurityGroup operation creates a new cache security group.
-- Use a cache security group to control access to one or more cache clusters.
-- Cache security groups are only used when you are creating a cluster outside
-- of an Amazon Virtual Private Cloud (VPC). If you are creating a cluster
-- inside of a VPC, use a cache subnet group instead. For more information,
-- see CreateCacheSubnetGroup. https://elasticache.us-east-1.amazonaws.com/
-- ?Action=CreateCacheSecurityGroup
-- &CacheSecurityGroupName=mycachesecuritygroup
-- &Description=My%20cache%20security%20group &Version=2013-06-15
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-07-27T02%3A43%3A10.703Z &AWSAccessKeyId=YOUR-ACCESS-KEY
-- &Signature=YOUR-SIGNATURE mycachesecuritygroup 123456789012 My cache
-- security group 2b1c8035-b7fa-11e0-9326-b7275b9d4a6c.
module Network.AWS.ElastiCache.CreateCacheSecurityGroup where

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

data CreateCacheSecurityGroup = CreateCacheSecurityGroup
    { ccsgmCacheSecurityGroupName :: !Text
      -- ^ A name for the cache security group. This value is stored as a lowercase
      -- string. Constraints: Must contain no more than 255 alphanumeric characters.
      -- Must not be the word "Default". Example: mysecuritygroup.
    , ccsgmDescription :: !Text
      -- ^ A description for the cache security group.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateCacheSecurityGroup

instance AWSRequest CreateCacheSecurityGroup where
    type Er CreateCacheSecurityGroup = ElastiCacheError
    type Rs CreateCacheSecurityGroup = CreateCacheSecurityGroupResponse
    request = getQuery service "CreateCacheSecurityGroup"

data CreateCacheSecurityGroupResponse = CreateCacheSecurityGroupResponse
    { ccsgmrsCacheSecurityGroup :: Maybe CacheSecurityGroup
      -- ^ Represents the output of one of the following operations:
      -- AuthorizeCacheSecurityGroupIngress CreateCacheSecurityGroup
      -- RevokeCacheSecurityGroupIngress.
    } deriving (Eq, Show, Generic)

instance FromXML CreateCacheSecurityGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateCacheSecurityGroupResponse"
        :| ["CreateCacheSecurityGroupResult"]
