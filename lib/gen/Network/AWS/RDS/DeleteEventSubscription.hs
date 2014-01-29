{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DeleteEventSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes an RDS event notification subscription.
-- https://rds.us-east-1.amazonaws.com/ ?Action=DeleteEventSubscription
-- &SubscriptionName=EventSubscription01 &Version=2013-01-10
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20130128T012739Z
-- &AWSAccessKeyId= &Signature= true 012345678901 db-instance deleting
-- 2013-01-28 00:29:23.736 creation deletion EventSubscription01
-- arn:aws:sns:us-east-1:012345678901:EventSubscription01
-- e7cf30ac-68e9-11e2-bd13-a92da73b3119.
module Network.AWS.RDS.DeleteEventSubscription where

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
deleteEventSubscription :: Text
                        -> DeleteEventSubscription
deleteEventSubscription p1 = undefined $ DeleteEventSubscription
    { desmSubscriptionName = p1
    }

data DeleteEventSubscription = DeleteEventSubscription
    { desmSubscriptionName :: !Text
      -- ^ The name of the RDS event notification subscription you want to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteEventSubscription

instance AWSRequest DeleteEventSubscription where
    type Er DeleteEventSubscription = RDSError
    type Rs DeleteEventSubscription = DeleteEventSubscriptionResponse
    request = getQuery service "DeleteEventSubscription"

data DeleteEventSubscriptionResponse = DeleteEventSubscriptionResponse
    { desmrsEventSubscription :: Maybe EventSubscription
      -- ^ Contains the results of a successful invocation of the
      -- DescribeEventSubscriptions action.
    } deriving (Eq, Show, Generic)

instance FromXML DeleteEventSubscriptionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeleteEventSubscriptionResponse"
        :| ["DeleteEventSubscriptionResult"]
