{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.AuthorizeCacheSecurityGroupIngress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The AuthorizeCacheSecurityGroupIngress operation allows network ingress to
-- a cache security group. Applications using ElastiCache must be running on
-- Amazon EC2, and Amazon EC2 security groups are used as the authorization
-- mechanism. You cannot authorize ingress from an Amazon EC2 security group
-- in one Region to an ElastiCache cluster in another Region.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=AuthorizeCacheSecurityGroupIngress &EC2SecurityGroupName=default
-- &CacheSecurityGroupName=mygroup &EC2SecurityGroupOwnerId=1234-5678-1234
-- &Version=2013-06-15 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-02-12T01%3A29%3A15.746Z &AWSAccessKeyId=YOUR-ACCESS-KEY
-- &Signature=YOUR-SIGNATURE authorizing default 565419523791 mygroup
-- 123456781234 My security group 817fa999-3647-11e0-ae57-f96cfe56749c.
module Network.AWS.ElastiCache.AuthorizeCacheSecurityGroupIngress where

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
authorizeCacheSecurityGroupIngress :: Text
                                   -> Text
                                   -> Text
                                   -> AuthorizeCacheSecurityGroupIngress
authorizeCacheSecurityGroupIngress p1 p2 p3 = AuthorizeCacheSecurityGroupIngress
    { acsgimCacheSecurityGroupName = p1
    , acsgimEC2SecurityGroupName = p2
    , acsgimEC2SecurityGroupOwnerId = p3
    }

data AuthorizeCacheSecurityGroupIngress = AuthorizeCacheSecurityGroupIngress
    { acsgimCacheSecurityGroupName :: !Text
      -- ^ The cache security group which will allow network ingress.
    , acsgimEC2SecurityGroupName :: !Text
      -- ^ The Amazon EC2 security group to be authorized for ingress to the cache
      -- security group.
    , acsgimEC2SecurityGroupOwnerId :: !Text
      -- ^ The AWS account number of the Amazon EC2 security group owner. Note that
      -- this is not the same thing as an AWS access key ID - you must provide a
      -- valid AWS account number for this parameter.
    } deriving (Eq, Show, Generic)

instance ToQuery AuthorizeCacheSecurityGroupIngress

instance AWSRequest AuthorizeCacheSecurityGroupIngress where
    type Er AuthorizeCacheSecurityGroupIngress = ElastiCacheError
    type Rs AuthorizeCacheSecurityGroupIngress = AuthorizeCacheSecurityGroupIngressResponse
    request = getQuery service "AuthorizeCacheSecurityGroupIngress"

data AuthorizeCacheSecurityGroupIngressResponse = AuthorizeCacheSecurityGroupIngressResponse
    { acsgimrsCacheSecurityGroup :: Maybe CacheSecurityGroup
      -- ^ Represents the output of one of the following operations:
      -- AuthorizeCacheSecurityGroupIngress CreateCacheSecurityGroup
      -- RevokeCacheSecurityGroupIngress.
    } deriving (Eq, Show, Generic)

instance FromXML AuthorizeCacheSecurityGroupIngressResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "AuthorizeCacheSecurityGroupIngressResponse"
        :| ["AuthorizeCacheSecurityGroupIngressResult"]
