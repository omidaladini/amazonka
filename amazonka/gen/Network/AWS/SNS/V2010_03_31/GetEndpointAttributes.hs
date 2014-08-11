{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SNS.V2010_03_31.GetEndpointAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves the endpoint attributes for a device on one of the supported push
-- notification services, such as GCM and APNS. For more information, see
-- Using Amazon SNS Mobile Push Notifications. POST
-- http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ...
-- Action=GetEndpointAttributes &amp;SignatureMethod=HmacSHA256
-- &amp;AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &amp;EndpointArn=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Aendpoint%2FGCM%2Fgcmpushapp%2F5e3e9847-3183-3f18-a7e8-671c3a57d4b3
-- &amp;SignatureVersion=2 &amp;Version=2010-03-31
-- &amp;Signature=%2B2egbEoT4npw3p5H3wiIdzZBoTn4KI3UWmMFyBsHH9c%3D
-- &amp;Timestamp=2013-07-01T22%3A44%3A56.515Z HTTP/1.1 200 OK ...
-- &lt;GetEndpointAttributesResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;GetEndpointAttributesResult&gt; &lt;Attributes&gt; &lt;entry&gt;
-- &lt;key&gt;Enabled&lt;/key&gt; &lt;value&gt;true&lt;/value&gt;
-- &lt;/entry&gt; &lt;entry&gt; &lt;key&gt;CustomUserData&lt;/key&gt;
-- &lt;value&gt;UserId=01234567&lt;/value&gt; &lt;/entry&gt; &lt;entry&gt;
-- &lt;key&gt;Token&lt;/key&gt;
-- &lt;value&gt;APA91bGi7fFachkC1xjlqT66VYEucGHochmf1VQAr9k...jsM0PKPxKhddCzx6paEsyay9Zn3D4wNUJb8m6HZrBEXAMPLE&lt;/value&gt;
-- &lt;/entry&gt; &lt;/Attributes&gt; &lt;/GetEndpointAttributesResult&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;6c725a19-a142-5b77-94f9-1055a9ea04e7&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/GetEndpointAttributesResponse&gt;.
module Network.AWS.SNS.V2010_03_31.GetEndpointAttributes where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.SNS.V2010_03_31.Types
import Network.AWS.Prelude

data GetEndpointAttributes = GetEndpointAttributes
    { _geaiEndpointArn :: Text
      -- ^ EndpointArn for GetEndpointAttributes input.
    } deriving (Show, Generic)

makeLenses ''GetEndpointAttributes

instance ToQuery GetEndpointAttributes where
    toQuery = genericToQuery def

data GetEndpointAttributesResponse = GetEndpointAttributesResponse
    { _gearAttributes :: HashMap Text Text
      -- ^ Attributes include the following: CustomUserData -- arbitrary
      -- user data to associate with the endpoint. Amazon SNS does not use
      -- this data. The data must be in UTF-8 format and less than 2KB.
      -- Enabled -- flag that enables/disables delivery to the endpoint.
      -- Amazon SNS will set this to false when a notification service
      -- indicates to Amazon SNS that the endpoint is invalid. Users can
      -- set it back to true, typically after updating Token. Token --
      -- device token, also referred to as a registration id, for an app
      -- and mobile device. This is returned from the notification service
      -- when an app and mobile device are registered with the
      -- notification service.
    } deriving (Show, Generic)

makeLenses ''GetEndpointAttributesResponse

instance AWSRequest GetEndpointAttributes where
    type Sv GetEndpointAttributes = SNS
    type Rs GetEndpointAttributes = GetEndpointAttributesResponse

    request = post "GetEndpointAttributes"
    response _ = cursorResponse $ \hs xml ->
        pure GetEndpointAttributesResponse
            <*> xml %| "MapStringToString"