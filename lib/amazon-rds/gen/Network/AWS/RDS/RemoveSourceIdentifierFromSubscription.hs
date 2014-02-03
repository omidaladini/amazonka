{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.RemoveSourceIdentifierFromSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes a source identifier from an existing RDS event notification
-- subscription. https://rds.us-east-1.amazonaws.com/
-- ?Action=RemoveSourceIdentifierFromSubscription
-- &SubscriptionName=EventSubscription01 &SourceIdentifier=dbinstance01
-- &Version=2013-01-10 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20130128T012415Z &AWSAccessKeyId= &Signature= true 012345678901
-- db-instance modifying 2013-01-28 00:29:23.736 creation deletion
-- EventSubscription01 arn:aws:sns:us-east-1:012345678901:EventSubscription01
-- 6f0b82bf-68e9-11e2-b97b-43c6362ec60d.
module Network.AWS.RDS.RemoveSourceIdentifierFromSubscription where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
removeSourceIdentifierFromSubscription :: Text
                                       -> Text
                                       -> RemoveSourceIdentifierFromSubscription
removeSourceIdentifierFromSubscription p1 p2 = RemoveSourceIdentifierFromSubscription
    { rsifsmSourceIdentifier = p1
    , rsifsmSubscriptionName = p2
    }

data RemoveSourceIdentifierFromSubscription = RemoveSourceIdentifierFromSubscription
    { rsifsmSourceIdentifier :: !Text
      -- ^ The source identifier to be removed from the subscription, such as the DB
      -- instance identifier for a DB instance or the name of a security group.
    , rsifsmSubscriptionName :: !Text
      -- ^ The name of the RDS event notification subscription you want to remove a
      -- source identifier from.
    } deriving (Eq, Show, Generic)

instance ToQuery RemoveSourceIdentifierFromSubscription

instance AWSRequest RemoveSourceIdentifierFromSubscription where
    type Er RemoveSourceIdentifierFromSubscription = RDSError
    type Rs RemoveSourceIdentifierFromSubscription = RemoveSourceIdentifierFromSubscriptionResponse
    request = getQuery service "RemoveSourceIdentifierFromSubscription"

data RemoveSourceIdentifierFromSubscriptionResponse = RemoveSourceIdentifierFromSubscriptionResponse
    { rsifsmrsEventSubscription :: Maybe EventSubscription
      -- ^ Contains the results of a successful invocation of the
      -- DescribeEventSubscriptions action.
    } deriving (Eq, Show, Generic)

instance FromXML RemoveSourceIdentifierFromSubscriptionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "RemoveSourceIdentifierFromSubscriptionResponse"
        :| ["RemoveSourceIdentifierFromSubscriptionResult"]
