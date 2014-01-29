{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.SetIdentityNotificationTopic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Given an identity (email address or domain), sets the Amazon SNS topic to
-- which Amazon SES will publish bounce and complaint notifications for emails
-- sent with that identity as the Source. Publishing to topics may only be
-- disabled when feedback forwarding is enabled. This action is throttled at
-- one request per second. For more information about feedback notification,
-- see the Amazon SES Developer Guide. POST / HTTP/1.1 Date: Sat, 12 May 2012
-- 05:27:54 GMT Host: email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=3+KQ4VHx991T7Kb41HmFcZJxuHz4/6mf2H5FxY+tuLc=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 203
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=SetIdentityNotificationTopic
-- &Identity=user@example.com
-- &SnsTopic=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3Aexample
-- &NotificationType=Bounce
-- &Timestamp=2012-05-12T05%3A27%3A54.000Z&Version=2010-12-01
-- 299f4af4-b72a-11e1-901f-1fbd90e8104f.
module Network.AWS.SES.SetIdentityNotificationTopic where

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

import Network.AWS.SES.Service
import Network.AWS.SES.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
setIdentityNotificationTopic :: Text
                             -> NotificationType
                             -> SetIdentityNotificationTopic
setIdentityNotificationTopic p1 p2 = undefined $ SetIdentityNotificationTopic
    { sintrIdentity = p1
    , sintrNotificationType = p2
    , sintrSnsTopic = Nothing
    }

data SetIdentityNotificationTopic = SetIdentityNotificationTopic
    { sintrIdentity :: !Text
      -- ^ The identity for which the topic will be set. Examples: user@example.com,
      -- example.com.
    , sintrNotificationType :: !NotificationType
      -- ^ The type of feedback notifications that will be published to the specified
      -- topic.
    , sintrSnsTopic :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
      -- (Amazon SNS) topic. If the parameter is omitted from the request or a null
      -- value is passed, the topic is cleared and publishing is disabled.
    } deriving (Eq, Show, Generic)

instance ToQuery SetIdentityNotificationTopic

instance AWSRequest SetIdentityNotificationTopic where
    type Er SetIdentityNotificationTopic = SESError
    type Rs SetIdentityNotificationTopic = SetIdentityNotificationTopicResponse
    request = getQuery service "SetIdentityNotificationTopic"

data SetIdentityNotificationTopicResponse = SetIdentityNotificationTopicResponse
    deriving (Eq, Show, Generic)

instance FromXML SetIdentityNotificationTopicResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "SetIdentityNotificationTopicResponse"
        :| ["SetIdentityNotificationTopicResult"]
