{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS
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
module Network.AWS.RDS
    (
    -- * Request
      CreateEventSubscription
    -- ** Request constructor
    , mkCreateEventSubscription
    -- ** Request lenses
    , cesSubscriptionName
    , cesSnsTopicArn
    , cesSourceType
    , cesEventCategories
    , cesSourceIds
    , cesEnabled
    , cesTags

    -- * Response
    , CreateEventSubscriptionResponse
    -- ** Response constructor
    , mkCreateEventSubscriptionResponse
    -- ** Response lenses
    , cesrEventSubscription
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

-- | 
data CreateEventSubscription = CreateEventSubscription
    { _cesSubscriptionName :: !Text
    , _cesSnsTopicArn :: !Text
    , _cesSourceType :: !(Maybe Text)
    , _cesEventCategories :: [Text]
    , _cesSourceIds :: [Text]
    , _cesEnabled :: !(Maybe Bool)
    , _cesTags :: [Tag]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateEventSubscription' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SubscriptionName ::@ @Text@
--
-- * @SnsTopicArn ::@ @Text@
--
-- * @SourceType ::@ @Maybe Text@
--
-- * @EventCategories ::@ @[Text]@
--
-- * @SourceIds ::@ @[Text]@
--
-- * @Enabled ::@ @Maybe Bool@
--
-- * @Tags ::@ @[Tag]@
--
mkCreateEventSubscription :: Text -- ^ 'cesSubscriptionName'
                          -> Text -- ^ 'cesSnsTopicArn'
                          -> CreateEventSubscription
mkCreateEventSubscription p1 p2 = CreateEventSubscription
    { _cesSubscriptionName = p1
    , _cesSnsTopicArn = p2
    , _cesSourceType = Nothing
    , _cesEventCategories = mempty
    , _cesSourceIds = mempty
    , _cesEnabled = Nothing
    , _cesTags = mempty
    }

-- | The name of the subscription. Constraints: The name must be less than 255
-- characters.
cesSubscriptionName :: Lens' CreateEventSubscription Text
cesSubscriptionName =
    lens _cesSubscriptionName (\s a -> s { _cesSubscriptionName = a })

-- | The Amazon Resource Name (ARN) of the SNS topic created for event
-- notification. The ARN is created by Amazon SNS when you create a topic and
-- subscribe to it.
cesSnsTopicArn :: Lens' CreateEventSubscription Text
cesSnsTopicArn = lens _cesSnsTopicArn (\s a -> s { _cesSnsTopicArn = a })

-- | The type of source that will be generating the events. For example, if you
-- want to be notified of events generated by a DB instance, you would set
-- this parameter to db-instance. if this value is not specified, all events
-- are returned. Valid values: db-instance | db-parameter-group |
-- db-security-group | db-snapshot.
cesSourceType :: Lens' CreateEventSubscription (Maybe Text)
cesSourceType = lens _cesSourceType (\s a -> s { _cesSourceType = a })

-- | A list of event categories for a SourceType that you want to subscribe to.
-- You can see a list of the categories for a given SourceType in the Events
-- topic in the Amazon RDS User Guide or by using the DescribeEventCategories
-- action.
cesEventCategories :: Lens' CreateEventSubscription [Text]
cesEventCategories =
    lens _cesEventCategories (\s a -> s { _cesEventCategories = a })

-- | The list of identifiers of the event sources for which events will be
-- returned. If not specified, then all sources are included in the response.
-- An identifier must begin with a letter and must contain only ASCII letters,
-- digits, and hyphens; it cannot end with a hyphen or contain two consecutive
-- hyphens. Constraints: If SourceIds are supplied, SourceType must also be
-- provided. If the source type is a DB instance, then a DBInstanceIdentifier
-- must be supplied. If the source type is a DB security group, a
-- DBSecurityGroupName must be supplied. If the source type is a DB parameter
-- group, a DBParameterGroupName must be supplied. If the source type is a DB
-- snapshot, a DBSnapshotIdentifier must be supplied.
cesSourceIds :: Lens' CreateEventSubscription [Text]
cesSourceIds = lens _cesSourceIds (\s a -> s { _cesSourceIds = a })

-- | A Boolean value; set to true to activate the subscription, set to false to
-- create the subscription but not active it.
cesEnabled :: Lens' CreateEventSubscription (Maybe Bool)
cesEnabled = lens _cesEnabled (\s a -> s { _cesEnabled = a })

-- | A list of tags.
cesTags :: Lens' CreateEventSubscription [Tag]
cesTags = lens _cesTags (\s a -> s { _cesTags = a })

instance ToQuery CreateEventSubscription where
    toQuery = genericQuery def

newtype CreateEventSubscriptionResponse = CreateEventSubscriptionResponse
    { _cesrEventSubscription :: Maybe EventSubscription
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateEventSubscriptionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @EventSubscription ::@ @Maybe EventSubscription@
--
mkCreateEventSubscriptionResponse :: CreateEventSubscriptionResponse
mkCreateEventSubscriptionResponse = CreateEventSubscriptionResponse
    { _cesrEventSubscription = Nothing
    }

-- | Contains the results of a successful invocation of the
-- DescribeEventSubscriptions action.
cesrEventSubscription :: Lens' CreateEventSubscriptionResponse (Maybe EventSubscription)
cesrEventSubscription =
    lens _cesrEventSubscription (\s a -> s { _cesrEventSubscription = a })

instance FromXML CreateEventSubscriptionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateEventSubscription where
    type Sv CreateEventSubscription = RDS
    type Rs CreateEventSubscription = CreateEventSubscriptionResponse

    request = post "CreateEventSubscription"
    response _ = xmlResponse
