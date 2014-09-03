{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.ModifyEventSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies an existing Amazon Redshift event notification subscription.
module Network.AWS.Redshift.V2012_12_01.ModifyEventSubscription
    (
    -- * Request
      ModifyEventSubscription
    -- ** Request constructor
    , modifyEventSubscription
    -- ** Request lenses
    , mesmSubscriptionName
    , mesmEnabled
    , mesmEventCategories
    , mesmSourceIds
    , mesmSnsTopicArn
    , mesmSourceType
    , mesmSeverity

    -- * Response
    , ModifyEventSubscriptionResponse
    -- ** Response lenses
    , esxEventSubscription
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ModifyEventSubscription' request.
modifyEventSubscription :: Text -- ^ 'mesmSubscriptionName'
                        -> ModifyEventSubscription
modifyEventSubscription p1 = ModifyEventSubscription
    { _mesmSubscriptionName = p1
    , _mesmEnabled = Nothing
    , _mesmEventCategories = mempty
    , _mesmSourceIds = mempty
    , _mesmSnsTopicArn = Nothing
    , _mesmSourceType = Nothing
    , _mesmSeverity = Nothing
    }

data ModifyEventSubscription = ModifyEventSubscription
    { _mesmSubscriptionName :: Text
      -- ^ The name of the modified Amazon Redshift event notification
      -- subscription.
    , _mesmEnabled :: Maybe Bool
      -- ^ A Boolean value indicating if the subscription is enabled. true
      -- indicates the subscription is enabled.
    , _mesmEventCategories :: [Text]
      -- ^ Specifies the Amazon Redshift event categories to be published by
      -- the event notification subscription. Values: Configuration,
      -- Management, Monitoring, Security.
    , _mesmSourceIds :: [Text]
      -- ^ A list of one or more identifiers of Amazon Redshift source
      -- objects. All of the objects must be of the same type as was
      -- specified in the source type parameter. The event subscription
      -- will return only events generated by the specified objects. If
      -- not specified, then events are returned for all objects within
      -- the source type specified. Example: my-cluster-1, my-cluster-2
      -- Example: my-snapshot-20131010.
    , _mesmSnsTopicArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the SNS topic to be used by the
      -- event notification subscription.
    , _mesmSourceType :: Maybe Text
      -- ^ The type of source that will be generating the events. For
      -- example, if you want to be notified of events generated by a
      -- cluster, you would set this parameter to cluster. If this value
      -- is not specified, events are returned for all Amazon Redshift
      -- objects in your AWS account. You must specify a source type in
      -- order to specify source IDs. Valid values: cluster,
      -- cluster-parameter-group, cluster-security-group, and
      -- cluster-snapshot.
    , _mesmSeverity :: Maybe Text
      -- ^ Specifies the Amazon Redshift event severity to be published by
      -- the event notification subscription. Values: ERROR, INFO.
    } deriving (Show, Generic)

-- | The name of the modified Amazon Redshift event notification subscription.
mesmSubscriptionName
    :: Functor f
    => (Text
    -> f (Text))
    -> ModifyEventSubscription
    -> f ModifyEventSubscription
mesmSubscriptionName f x =
    (\y -> x { _mesmSubscriptionName = y })
       <$> f (_mesmSubscriptionName x)
{-# INLINE mesmSubscriptionName #-}

-- | A Boolean value indicating if the subscription is enabled. true indicates
-- the subscription is enabled.
mesmEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> ModifyEventSubscription
    -> f ModifyEventSubscription
mesmEnabled f x =
    (\y -> x { _mesmEnabled = y })
       <$> f (_mesmEnabled x)
{-# INLINE mesmEnabled #-}

-- | Specifies the Amazon Redshift event categories to be published by the event
-- notification subscription. Values: Configuration, Management, Monitoring,
-- Security.
mesmEventCategories
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> ModifyEventSubscription
    -> f ModifyEventSubscription
mesmEventCategories f x =
    (\y -> x { _mesmEventCategories = y })
       <$> f (_mesmEventCategories x)
{-# INLINE mesmEventCategories #-}

-- | A list of one or more identifiers of Amazon Redshift source objects. All of
-- the objects must be of the same type as was specified in the source type
-- parameter. The event subscription will return only events generated by the
-- specified objects. If not specified, then events are returned for all
-- objects within the source type specified. Example: my-cluster-1,
-- my-cluster-2 Example: my-snapshot-20131010.
mesmSourceIds
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> ModifyEventSubscription
    -> f ModifyEventSubscription
mesmSourceIds f x =
    (\y -> x { _mesmSourceIds = y })
       <$> f (_mesmSourceIds x)
{-# INLINE mesmSourceIds #-}

-- | The Amazon Resource Name (ARN) of the SNS topic to be used by the event
-- notification subscription.
mesmSnsTopicArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ModifyEventSubscription
    -> f ModifyEventSubscription
mesmSnsTopicArn f x =
    (\y -> x { _mesmSnsTopicArn = y })
       <$> f (_mesmSnsTopicArn x)
{-# INLINE mesmSnsTopicArn #-}

-- | The type of source that will be generating the events. For example, if you
-- want to be notified of events generated by a cluster, you would set this
-- parameter to cluster. If this value is not specified, events are returned
-- for all Amazon Redshift objects in your AWS account. You must specify a
-- source type in order to specify source IDs. Valid values: cluster,
-- cluster-parameter-group, cluster-security-group, and cluster-snapshot.
mesmSourceType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ModifyEventSubscription
    -> f ModifyEventSubscription
mesmSourceType f x =
    (\y -> x { _mesmSourceType = y })
       <$> f (_mesmSourceType x)
{-# INLINE mesmSourceType #-}

-- | Specifies the Amazon Redshift event severity to be published by the event
-- notification subscription. Values: ERROR, INFO.
mesmSeverity
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ModifyEventSubscription
    -> f ModifyEventSubscription
mesmSeverity f x =
    (\y -> x { _mesmSeverity = y })
       <$> f (_mesmSeverity x)
{-# INLINE mesmSeverity #-}

instance ToQuery ModifyEventSubscription where
    toQuery = genericQuery def

data ModifyEventSubscriptionResponse = ModifyEventSubscriptionResponse
    { _esxEventSubscription :: Maybe EventSubscription
      -- ^ 
    } deriving (Show, Generic)

-- | 
esxEventSubscription
    :: Functor f
    => (Maybe EventSubscription
    -> f (Maybe EventSubscription))
    -> ModifyEventSubscriptionResponse
    -> f ModifyEventSubscriptionResponse
esxEventSubscription f x =
    (\y -> x { _esxEventSubscription = y })
       <$> f (_esxEventSubscription x)
{-# INLINE esxEventSubscription #-}

instance FromXML ModifyEventSubscriptionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyEventSubscription where
    type Sv ModifyEventSubscription = Redshift
    type Rs ModifyEventSubscription = ModifyEventSubscriptionResponse

    request = post "ModifyEventSubscription"
    response _ = xmlResponse
