{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.CreateEventSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an Amazon Redshift event notification subscription. This action
-- requires an ARN (Amazon Resource Name) of an Amazon SNS topic created by
-- either the Amazon Redshift console, the Amazon SNS console, or the Amazon
-- SNS API. To obtain an ARN with Amazon SNS, you must create a topic in
-- Amazon SNS and subscribe to the topic. The ARN is displayed in the SNS
-- console. You can specify the source type, and lists of Amazon Redshift
-- source IDs, event categories, and event severities. Notifications will be
-- sent for all events you want that match those criteria. For example, you
-- can specify source type = cluster, source ID = my-cluster-1 and mycluster2,
-- event categories = Availability, Backup, and severity = ERROR. The
-- subsription will only send notifications for those ERROR events in the
-- Availability and Backup categores for the specified clusters. If you
-- specify both the source type and source IDs, such as source type = cluster
-- and source identifier = my-cluster-1, notifiactions will be sent for all
-- the cluster events for my-cluster-1. If you specify a source type but do
-- not specify a source identifier, you will receive notice of the events for
-- the objects of that type in your AWS account. If you do not specify either
-- the SourceType nor the SourceIdentifier, you will be notified of events
-- generated from all Amazon Redshift sources belonging to your AWS account.
-- You must specify a source type if you specify a source ID.
module Network.AWS.Redshift.CreateEventSubscription where

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

import Network.AWS.Redshift.Service
import Network.AWS.Redshift.Types

-- | Convenience method utilising default fields where applicable.
createEventSubscription :: Text
                        -> Text
                        -> AWS (Either RedshiftError CreateEventSubscriptionResponse)
createEventSubscription p1 p2 = undefined $ CreateEventSubscription
    { cesmSnsTopicArn = p1
    , cesmSubscriptionName = p2
    , cesmEnabled = Nothing
    , cesmEventCategories = []
    , cesmSeverity = Nothing
    , cesmSourceIds = []
    , cesmSourceType = Nothing
    }

data CreateEventSubscription = CreateEventSubscription
    { cesmEnabled :: Maybe Bool
      -- ^ A Boolean value; set to true to activate the subscription, set to false to
      -- create the subscription but not active it.
    , cesmEventCategories :: [Text]
      -- ^ Specifies the Amazon Redshift event categories to be published by the event
      -- notification subscription. Values: Configuration, Management, Monitoring,
      -- Security.
    , cesmSeverity :: Maybe Text
      -- ^ Specifies the Amazon Redshift event severity to be published by the event
      -- notification subscription. Values: ERROR, INFO.
    , cesmSnsTopicArn :: !Text
      -- ^ The Amazon Resource Name (ARN) of the Amazon SNS topic used to transmit the
      -- event notifications. The ARN is created by Amazon SNS when you create a
      -- topic and subscribe to it.
    , cesmSourceIds :: [Text]
      -- ^ A list of one or more identifiers of Amazon Redshift source objects. All of
      -- the objects must be of the same type as was specified in the source type
      -- parameter. The event subscription will return only events generated by the
      -- specified objects. If not specified, then events are returned for all
      -- objects within the source type specified. Example: my-cluster-1,
      -- my-cluster-2 Example: my-snapshot-20131010.
    , cesmSourceType :: Maybe Text
      -- ^ The type of source that will be generating the events. For example, if you
      -- want to be notified of events generated by a cluster, you would set this
      -- parameter to cluster. If this value is not specified, events are returned
      -- for all Amazon Redshift objects in your AWS account. You must specify a
      -- source type in order to specify source IDs. Valid values: cluster,
      -- cluster-parameter-group, cluster-security-group, and cluster-snapshot.
    , cesmSubscriptionName :: !Text
      -- ^ The name of the event subscription to be created. Constraints: Cannot be
      -- null, empty, or blank. Must contain from 1 to 255 alphanumeric characters
      -- or hyphens. First character must be a letter. Cannot end with a hyphen or
      -- contain two consecutive hyphens.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateEventSubscription

instance AWSRequest CreateEventSubscription where
    type Er CreateEventSubscription = RedshiftError
    type Rs CreateEventSubscription = CreateEventSubscriptionResponse
    request = getQuery service "CreateEventSubscription"

data CreateEventSubscriptionResponse = CreateEventSubscriptionResponse
    { cesmrsEventSubscription :: Maybe EventSubscription
    } deriving (Eq, Show, Generic)

instance FromXML CreateEventSubscriptionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateEventSubscriptionResponse"
        :| ["CreateEventSubscriptionResult"]
