{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.CreateEventSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an RDS event notification subscription. This action requires a
-- topic ARN (Amazon Resource Name) created by either the RDS console, the SNS
-- console, or the SNS API. To obtain an ARN with SNS, you must create a topic
-- in Amazon SNS and subscribe to the topic. The ARN is displayed in the SNS
-- console. You can specify the type of source (SourceType) you want to be
-- notified of, provide a list of RDS sources (SourceIds) that triggers the
-- events, and provide a list of event categories (EventCategories) for events
-- you want to be notified of. For example, you can specify SourceType =
-- db-instance, SourceIds = mydbinstance1, mydbinstance2 and EventCategories =
-- Availability, Backup. If you specify both the SourceType and SourceIds,
-- such as SourceType = db-instance and SourceIdentifier = myDBInstance1, you
-- will be notified of all the db-instance events for the specified source. If
-- you specify a SourceType but do not specify a SourceIdentifier, you will
-- receive notice of the events for that source type for all your RDS sources.
-- If you do not specify either the SourceType nor the SourceIdentifier, you
-- will be notified of events generated from all RDS sources belonging to your
-- customer account. https://rds.us-east-1.amazonaws.com/
-- ?Action=CreateEventSubscription &SubscriptionName=EventSubscription02
-- &Enabled=true
-- &SnsTopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A012345678901%3AEventSubscription01
-- &Version=2013-01-10 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20130128T002941Z &AWSAccessKeyId= &Signature= true 012345678901
-- creating Mon Jan 28 00:29:42 UTC 2013 EventSubscription02
-- arn:aws:sns:us-east-1:012345678901:EventSubscription01
-- cf3407aa-68e1-11e2-bd13-a92da73b3119 https://rds.us-east-1.amazonaws.com/
-- ?Action=CreateEventSubscription &SubscriptionName=EventSubscription03
-- &SourceType=db-instance &EventCategories.member.1=creation
-- &EventCategories.member.2=deletion &SourceIds.member.1=dbinstance01
-- &SourceIds.member.2=dbinstance02 &Enabled=true
-- &SnsTopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A012345678901%3AEventSubscription01
-- &Version=2013-01-10 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20130128T014117Z &AWSAccessKeyId= &Signature= true 012345678901
-- db-instance creating dbinstance01 dbinstance02 Mon Jan 28 01:41:19 UTC 2013
-- creation deletion EventSubscription03
-- arn:aws:sns:us-east-1:012345678901:EventSubscription01
-- d064b48c-68eb-11e2-ab10-11125abcb784.
module Network.AWS.RDS.CreateEventSubscription where

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

-- | Convenience method utilising default fields where applicable.
createEventSubscription :: Text
                        -> Text
                        -> AWS (Either RDSError CreateEventSubscriptionResponse)
createEventSubscription p1 p2 = undefined $ CreateEventSubscription
    { cesmSnsTopicArn = p1
    , cesmSubscriptionName = p2
    , cesmEnabled = Nothing
    , cesmEventCategories = []
    , cesmSourceIds = []
    , cesmSourceType = Nothing
    , cesmTags = []
    }

data CreateEventSubscription = CreateEventSubscription
    { cesmEnabled :: Maybe Bool
      -- ^ A Boolean value; set to true to activate the subscription, set to false to
      -- create the subscription but not active it.
    , cesmEventCategories :: [Text]
      -- ^ A list of event categories for a SourceType that you want to subscribe to.
      -- You can see a list of the categories for a given SourceType in the Events
      -- topic in the Amazon RDS User Guide or by using the DescribeEventCategories
      -- action.
    , cesmSnsTopicArn :: !Text
      -- ^ The Amazon Resource Name (ARN) of the SNS topic created for event
      -- notification. The ARN is created by Amazon SNS when you create a topic and
      -- subscribe to it.
    , cesmSourceIds :: [Text]
      -- ^ The list of identifiers of the event sources for which events will be
      -- returned. If not specified, then all sources are included in the response.
      -- An identifier must begin with a letter and must contain only ASCII letters,
      -- digits, and hyphens; it cannot end with a hyphen or contain two consecutive
      -- hyphens. Constraints: If SourceIds are supplied, SourceType must also be
      -- provided. If the source type is a DB instance, then a DBInstanceIdentifier
      -- must be supplied. If the source type is a DB security group, a
      -- DBSecurityGroupName must be supplied. If the source type is a DB parameter
      -- group, a DBParameterGroupName must be supplied. If the source type is a DB
      -- snapshot, a DBSnapshotIdentifier must be supplied.
    , cesmSourceType :: Maybe Text
      -- ^ The type of source that will be generating the events. For example, if you
      -- want to be notified of events generated by a DB instance, you would set
      -- this parameter to db-instance. if this value is not specified, all events
      -- are returned. Valid values: db-instance | db-parameter-group |
      -- db-security-group | db-snapshot.
    , cesmSubscriptionName :: !Text
      -- ^ The name of the subscription. Constraints: The name must be less than 255
      -- characters.
    , cesmTags :: [Tag]
      -- ^ A list of tags.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateEventSubscription

instance AWSRequest CreateEventSubscription where
    type Er CreateEventSubscription = RDSError
    type Rs CreateEventSubscription = CreateEventSubscriptionResponse
    request = getQuery service "CreateEventSubscription"

data CreateEventSubscriptionResponse = CreateEventSubscriptionResponse
    { cesmrsEventSubscription :: Maybe EventSubscription
      -- ^ Contains the results of a successful invocation of the
      -- DescribeEventSubscriptions action.
    } deriving (Eq, Show, Generic)

instance FromXML CreateEventSubscriptionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateEventSubscriptionResponse"
        :| ["CreateEventSubscriptionResult"]
