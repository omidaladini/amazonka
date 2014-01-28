{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.ModifyEventSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies an existing RDS event notification subscription. Note that you
-- cannot modify the source identifiers using this call; to change source
-- identifiers for a subscription, use the AddSourceIdentifierToSubscription
-- and RemoveSourceIdentifierFromSubscription calls. You can see a list of the
-- event categories for a given SourceType in the Events topic in the Amazon
-- RDS User Guide or by using the DescribeEventCategories action.
-- https://rds.us-east-1.amazonaws.com/ ?Action=ModifyEventSubscription
-- &SubscriptionName=EventSubscription01 &EventCategories.member.1=creation
-- &EventCategories.member.2=deletion &SourceType=db-instance &Enabled=true
-- &Version=2013-01-10 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20130128T005359Z &AWSAccessKeyId= &Signature= true 012345678901
-- db-instance modifying 2013-01-28 00:29:23.736 creation deletion
-- EventSubscription01 arn:aws:sns:us-east-1:012345678901:EventSubscription01
-- 34907d48-68e5-11e2-98ef-2b071ac20a57.
module Network.AWS.RDS.ModifyEventSubscription where

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

data ModifyEventSubscription = ModifyEventSubscription
    { mesmEnabled :: Maybe Bool
      -- ^ A Boolean value; set to true to activate the subscription.
    , mesmEventCategories :: [Text]
      -- ^ A list of event categories for a SourceType that you want to subscribe to.
      -- You can see a list of the categories for a given SourceType in the Events
      -- topic in the Amazon RDS User Guide or by using the DescribeEventCategories
      -- action.
    , mesmSnsTopicArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the SNS topic created for event
      -- notification. The ARN is created by Amazon SNS when you create a topic and
      -- subscribe to it.
    , mesmSourceType :: Maybe Text
      -- ^ The type of source that will be generating the events. For example, if you
      -- want to be notified of events generated by a DB instance, you would set
      -- this parameter to db-instance. if this value is not specified, all events
      -- are returned. Valid values: db-instance | db-parameter-group |
      -- db-security-group | db-snapshot.
    , mesmSubscriptionName :: !Text
      -- ^ The name of the RDS event notification subscription.
    } deriving (Eq, Show, Generic)

instance ToQuery ModifyEventSubscription

instance AWSRequest ModifyEventSubscription where
    type Er ModifyEventSubscription = RDSError
    type Rs ModifyEventSubscription = ModifyEventSubscriptionResponse
    request = getQuery service "ModifyEventSubscription"

data ModifyEventSubscriptionResponse = ModifyEventSubscriptionResponse
    { mesmrsEventSubscription :: Maybe EventSubscription
      -- ^ Contains the results of a successful invocation of the
      -- DescribeEventSubscriptions action.
    } deriving (Eq, Show, Generic)

instance FromXML ModifyEventSubscriptionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ModifyEventSubscriptionResponse"
        :| ["ModifyEventSubscriptionResult"]
