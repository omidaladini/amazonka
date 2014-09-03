{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.V2010_03_31.CreatePlatformEndpoint
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an endpoint for a device and mobile app on one of the supported
-- push notification services, such as GCM and APNS. CreatePlatformEndpoint
-- requires the PlatformApplicationArn that is returned from
-- CreatePlatformApplication. The EndpointArn that is returned when using
-- CreatePlatformEndpoint can then be used by the Publish action to send a
-- message to a mobile app or by the Subscribe action for subscription to a
-- topic. The CreatePlatformEndpoint action is idempotent, so if the requester
-- already owns an endpoint with the same device token and attributes, that
-- endpoint's ARN is returned without creating a new endpoint. For more
-- information, see Using Amazon SNS Mobile Push Notifications. When using
-- CreatePlatformEndpoint with Baidu, two attributes must be provided:
-- ChannelId and UserId. The token field must also contain the ChannelId. For
-- more information, see Creating an Amazon SNS Endpoint for Baidu. POST
-- http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ...
-- PlatformApplicationArn=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Aapp%2FGCM%2Fgcmpushapp
-- &amp;Action=CreatePlatformEndpoint &amp;SignatureMethod=HmacSHA256
-- &amp;CustomUserData=UserId%3D27576823
-- &amp;AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &amp;Token=APA91bGi7fFachkC1xjlqT66VYEucGHochmf1VQAr9k...jsM0PKPxKhddCzx6paEsyay9Zn3D4wNUJb8m6HZrBEXAMPLE
-- &amp;SignatureVersion=2 &amp;Version=2010-03-31
-- &amp;Signature=Rg5vXBS6OfgPtWkt1u32p1w14uiGh%2BKOicvXNWTEz2w%3D
-- &amp;Timestamp=2013-07-01T15%3A49%3A50.598Z HTTP/1.1 200 OK ...
-- &lt;CreatePlatformEndpointResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;CreatePlatformEndpointResult&gt;
-- &lt;EndpointArn&gt;arn:aws:sns:us-west-2:123456789012:endpoint/GCM/gcmpushapp/5e3e9847-3183-3f18-a7e8-671c3a57d4b3&lt;/EndpointArn&gt;
-- &lt;/CreatePlatformEndpointResult&gt; &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;6613341d-3e15-53f7-bf3c-7e56994ba278&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/CreatePlatformEndpointResponse&gt;.
module Network.AWS.SNS.V2010_03_31.CreatePlatformEndpoint
    (
    -- * Request
      CreatePlatformEndpoint
    -- ** Request constructor
    , createPlatformEndpoint
    -- ** Request lenses
    , cpeiPlatformApplicationArn
    , cpeiToken
    , cpeiAttributes
    , cpeiCustomUserData

    -- * Response
    , CreatePlatformEndpointResponse
    -- ** Response lenses
    , cerEndpointArn
    ) where

import Network.AWS.Request.Query
import Network.AWS.SNS.V2010_03_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreatePlatformEndpoint' request.
createPlatformEndpoint :: Text -- ^ 'cpeiPlatformApplicationArn'
                       -> Text -- ^ 'cpeiToken'
                       -> CreatePlatformEndpoint
createPlatformEndpoint p1 p2 = CreatePlatformEndpoint
    { _cpeiPlatformApplicationArn = p1
    , _cpeiToken = p2
    , _cpeiAttributes = mempty
    , _cpeiCustomUserData = Nothing
    }

data CreatePlatformEndpoint = CreatePlatformEndpoint
    { _cpeiPlatformApplicationArn :: Text
      -- ^ PlatformApplicationArn returned from CreatePlatformApplication is
      -- used to create a an endpoint.
    , _cpeiToken :: Text
      -- ^ Unique identifier created by the notification service for an app
      -- on a device. The specific name for Token will vary, depending on
      -- which notification service is being used. For example, when using
      -- APNS as the notification service, you need the device token.
      -- Alternatively, when using GCM or ADM, the device token equivalent
      -- is called the registration ID.
    , _cpeiAttributes :: Map Text Text
      -- ^ For a list of attributes, see SetEndpointAttributes.
    , _cpeiCustomUserData :: Maybe Text
      -- ^ Arbitrary user data to associate with the endpoint. Amazon SNS
      -- does not use this data. The data must be in UTF-8 format and less
      -- than 2KB.
    } deriving (Show, Generic)

-- | PlatformApplicationArn returned from CreatePlatformApplication is used to
-- create a an endpoint.
cpeiPlatformApplicationArn
    :: Functor f
    => (Text
    -> f (Text))
    -> CreatePlatformEndpoint
    -> f CreatePlatformEndpoint
cpeiPlatformApplicationArn f x =
    (\y -> x { _cpeiPlatformApplicationArn = y })
       <$> f (_cpeiPlatformApplicationArn x)
{-# INLINE cpeiPlatformApplicationArn #-}

-- | Unique identifier created by the notification service for an app on a
-- device. The specific name for Token will vary, depending on which
-- notification service is being used. For example, when using APNS as the
-- notification service, you need the device token. Alternatively, when using
-- GCM or ADM, the device token equivalent is called the registration ID.
cpeiToken
    :: Functor f
    => (Text
    -> f (Text))
    -> CreatePlatformEndpoint
    -> f CreatePlatformEndpoint
cpeiToken f x =
    (\y -> x { _cpeiToken = y })
       <$> f (_cpeiToken x)
{-# INLINE cpeiToken #-}

-- | For a list of attributes, see SetEndpointAttributes.
cpeiAttributes
    :: Functor f
    => (Map Text Text
    -> f (Map Text Text))
    -> CreatePlatformEndpoint
    -> f CreatePlatformEndpoint
cpeiAttributes f x =
    (\y -> x { _cpeiAttributes = y })
       <$> f (_cpeiAttributes x)
{-# INLINE cpeiAttributes #-}

-- | Arbitrary user data to associate with the endpoint. Amazon SNS does not use
-- this data. The data must be in UTF-8 format and less than 2KB.
cpeiCustomUserData
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreatePlatformEndpoint
    -> f CreatePlatformEndpoint
cpeiCustomUserData f x =
    (\y -> x { _cpeiCustomUserData = y })
       <$> f (_cpeiCustomUserData x)
{-# INLINE cpeiCustomUserData #-}

instance ToQuery CreatePlatformEndpoint where
    toQuery = genericQuery def

data CreatePlatformEndpointResponse = CreatePlatformEndpointResponse
    { _cerEndpointArn :: Maybe Text
      -- ^ EndpointArn returned from CreateEndpoint action.
    } deriving (Show, Generic)

-- | EndpointArn returned from CreateEndpoint action.
cerEndpointArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreatePlatformEndpointResponse
    -> f CreatePlatformEndpointResponse
cerEndpointArn f x =
    (\y -> x { _cerEndpointArn = y })
       <$> f (_cerEndpointArn x)
{-# INLINE cerEndpointArn #-}

instance FromXML CreatePlatformEndpointResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreatePlatformEndpoint where
    type Sv CreatePlatformEndpoint = SNS
    type Rs CreatePlatformEndpoint = CreatePlatformEndpointResponse

    request = post "CreatePlatformEndpoint"
    response _ = xmlResponse
