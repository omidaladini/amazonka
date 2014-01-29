{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.RevokeCacheSecurityGroupIngress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The RevokeCacheSecurityGroupIngress operation revokes ingress from a cache
-- security group. Use this operation to disallow access from an Amazon EC2
-- security group that had been previously authorized.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=RevokeCacheSecurityGroupIngress &EC2SecurityGroupName=default
-- &CacheSecurityGroupName=mygroup &EC2SecurityGroupOwnerId=1234-5678-1234
-- &Version=2013-06-15 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-07-27T02%3A30%3A08.444Z &AWSAccessKeyId=YOUR-ACCESS-KEY
-- &Signature=YOUR-SIGNATURE revoking default 123456781234 mygroup
-- 123456789012 My security group 02ae3699-3650-11e0-a564-8f11342c56b0.
module Network.AWS.ElastiCache.RevokeCacheSecurityGroupIngress where

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
revokeCacheSecurityGroupIngress :: Text
                                -> Text
                                -> Text
                                -> RevokeCacheSecurityGroupIngress
revokeCacheSecurityGroupIngress p1 p2 p3 = undefined $ RevokeCacheSecurityGroupIngress
    { rcsgimCacheSecurityGroupName = p1
    , rcsgimEC2SecurityGroupName = p2
    , rcsgimEC2SecurityGroupOwnerId = p3
    }

data RevokeCacheSecurityGroupIngress = RevokeCacheSecurityGroupIngress
    { rcsgimCacheSecurityGroupName :: !Text
      -- ^ The name of the cache security group to revoke ingress from.
    , rcsgimEC2SecurityGroupName :: !Text
      -- ^ The name of the Amazon EC2 security group to revoke access from.
    , rcsgimEC2SecurityGroupOwnerId :: !Text
      -- ^ The AWS account number of the Amazon EC2 security group owner. Note that
      -- this is not the same thing as an AWS access key ID - you must provide a
      -- valid AWS account number for this parameter.
    } deriving (Eq, Show, Generic)

instance ToQuery RevokeCacheSecurityGroupIngress

instance AWSRequest RevokeCacheSecurityGroupIngress where
    type Er RevokeCacheSecurityGroupIngress = ElastiCacheError
    type Rs RevokeCacheSecurityGroupIngress = RevokeCacheSecurityGroupIngressResponse
    request = getQuery service "RevokeCacheSecurityGroupIngress"

data RevokeCacheSecurityGroupIngressResponse = RevokeCacheSecurityGroupIngressResponse
    { rcsgimrsCacheSecurityGroup :: Maybe CacheSecurityGroup
      -- ^ Represents the output of one of the following operations:
      -- AuthorizeCacheSecurityGroupIngress CreateCacheSecurityGroup
      -- RevokeCacheSecurityGroupIngress.
    } deriving (Eq, Show, Generic)

instance FromXML RevokeCacheSecurityGroupIngressResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "RevokeCacheSecurityGroupIngressResponse"
        :| ["RevokeCacheSecurityGroupIngressResult"]
