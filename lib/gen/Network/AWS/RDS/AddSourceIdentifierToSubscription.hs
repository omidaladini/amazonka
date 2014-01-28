{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.AddSourceIdentifierToSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds a source identifier to an existing RDS event notification
-- subscription. https://rds.us-east-1.amazonaws.com/
-- ?Action=AddSourceIdentifierToSubscription
-- ?SubscriptionName=EventSubscription01 &SourceIdentifier=dbinstance01
-- &Version=2013-01-10 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20130128T011410Z &AWSAccessKeyId= &Signature= true 012345678901
-- db-instance modifying dbinstance01 2013-01-28 00:29:23.736 creation
-- deletion EventSubscription01
-- arn:aws:sns:us-east-1:012345678901:EventSubscription01
-- 05d0fd8a-68e8-11e2-8e4d-31f087d822e1.
module Network.AWS.RDS.AddSourceIdentifierToSubscription where

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

import Network.AWS.RDS.Service
import Network.AWS.RDS.Types

data AddSourceIdentifierToSubscription = AddSourceIdentifierToSubscription
    { asitsmSourceIdentifier :: !Text
      -- ^ The identifier of the event source to be added. An identifier must begin
      -- with a letter and must contain only ASCII letters, digits, and hyphens; it
      -- cannot end with a hyphen or contain two consecutive hyphens. Constraints:
      -- If the source type is a DB instance, then a DBInstanceIdentifier must be
      -- supplied. If the source type is a DB security group, a DBSecurityGroupName
      -- must be supplied. If the source type is a DB parameter group, a
      -- DBParameterGroupName must be supplied. If the source type is a DB snapshot,
      -- a DBSnapshotIdentifier must be supplied.
    , asitsmSubscriptionName :: !Text
      -- ^ The name of the RDS event notification subscription you want to add a
      -- source identifier to.
    } deriving (Eq, Show, Generic)

instance ToQuery AddSourceIdentifierToSubscription

instance AWSRequest AddSourceIdentifierToSubscription where
    type Er AddSourceIdentifierToSubscription = RDSError
    type Rs AddSourceIdentifierToSubscription = AddSourceIdentifierToSubscriptionResponse
    request = getQuery service "AddSourceIdentifierToSubscription"

data AddSourceIdentifierToSubscriptionResponse = AddSourceIdentifierToSubscriptionResponse
    { asitsmrsEventSubscription :: Maybe EventSubscription
      -- ^ Contains the results of a successful invocation of the
      -- DescribeEventSubscriptions action.
    } deriving (Eq, Show, Generic)

instance FromXML AddSourceIdentifierToSubscriptionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "AddSourceIdentifierToSubscriptionResponse"
        :| ["AddSourceIdentifierToSubscriptionResult"]
