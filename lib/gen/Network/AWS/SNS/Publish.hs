{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.Publish
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The Publish action sends a message to all of a topic's subscribed
-- endpoints. When a messageId is returned, the message has been saved and
-- Amazon SNS will attempt to deliver it to the topic's subscribers shortly.
-- The format of the outgoing message to each subscribed endpoint depends on
-- the notification protocol selected. To use the Publish action for sending a
-- message to a mobile endpoint, such as an app on a Kindle device or mobile
-- phone, you must specify the EndpointArn. The EndpointArn is returned when
-- making a call with the CreatePlatformEndpoint action. The second example
-- below shows a request and response for publishing to a mobile endpoint. The
-- following example publishes the same message to all protocols:
-- http://sns.us-east-1.amazonaws.com/ ?Subject=My%20first%20message
-- &TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A698519295917%3AMy-Topic
-- &Message=Hello%20world%21 &Action=Publish &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2010-03-31T12%3A00%3A00.000Z
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &Signature=9GZysQ4Jpnz%2BHklqM7VFTvEcjR2LIUtn6jW47054xxE%3D Use the
-- following JSON object format for the Message parameter to send different
-- messages to each protocol (linebreaks added for readability): { "default" :
-- "some message", "email" : "some email message", "email-json" : "some
-- email-json message", "http" : "some http message", "https" : "some https
-- message", "sqs" : "some sqs message" } 94f20ce6-13c5-43a0-9a9e-ca52d816e90b
-- f187a3c1-376f-11df-8963-01868b7c937a POST
-- http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ... Action=Publish
-- &Message=%7B%22default%22%3A%22This+is+the+default+Message%22%2C%22APNS_SANDBOX%22%3A%22%7B+%5C%22aps%5C%22+%3A+%7B+%5C%22alert%5C%22+%3A+%5C%22You+have+got+email.%5C%22%2C+%5C%22badge%5C%22+%3A+9%2C%5C%22sound%5C%22+%3A%5C%22default%5C%22%7D%7D%22%7D
-- FIXME: Operation documentation for Publish
-- &TargetArn=arn%3Aaws%3Asns%3Aus-west-2%3A803981987763%3Aendpoint%2FAPNS_SANDBOX%2Fpushapp%2F98e9ced9-f136-3893-9d60-776547eafebb
-- &SignatureMethod=HmacSHA256 &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &SignatureVersion=2 &Version=2010-03-31
-- &Signature=vmqc4XRupKAxsDAdN4j4Ayw5LQljXMps3kss4bkDfCk%3D
-- &Timestamp=2013-07-18T22%3A44%3A09.452Z &MessageStructure=json HTTP/1.1 200
-- OK ... 567910cd-659e-55d4-8ccb-5aaf14679dc0
-- d74b8436-ae13-5ab4-a9ff-ce54dfea72a0.
module Network.AWS.SNS.Publish where

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

import Network.AWS.SNS.Service
import Network.AWS.SNS.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
publish :: Text
        -> Publish
publish p1 = undefined $ Publish
    { piMessage = p1
    , piMessageStructure = Nothing
    , piSubject = Nothing
    , piTargetArn = Nothing
    , piTopicArn = Nothing
    }

data Publish = Publish
    { piMessage :: !Text
      -- ^ The message you want to send to the topic. If you want to send the same
      -- message to all transport protocols, include the text of the message as a
      -- String value. If you want to send different messages for each transport
      -- protocol, set the value of the MessageStructure parameter to json and use a
      -- JSON object for the Message parameter. See the Examples section for the
      -- format of the JSON object. Constraints: Messages must be UTF-8 encoded
      -- strings at most 256 KB in size (262144 bytes, not 262144 characters).
      -- JSON-specific constraints: Keys in the JSON object that correspond to
      -- supported transport protocols must have simple JSON string values. The
      -- values will be parsed (unescaped) before they are used in outgoing
      -- messages. Outbound notifications are JSON encoded (meaning that the
      -- characters will be reescaped for sending). Values have a minimum length of
      -- 0 (the empty string, "", is allowed). Values have a maximum length bounded
      -- by the overall message size (so, including multiple protocols may limit
      -- message sizes). Non-string values will cause the key to be ignored. Keys
      -- that do not correspond to supported transport protocols are ignored.
      -- Duplicate keys are not allowed. Failure to parse or validate any key or
      -- value in the message will cause the Publish call to return an error (no
      -- partial delivery).
    , piMessageStructure :: Maybe Text
      -- ^ Set MessageStructure to json if you want to send a different message for
      -- each protocol. For example, using one publish action, you can send a short
      -- message to your SMS subscribers and a longer message to your email
      -- subscribers. If you set MessageStructure to json, the value of the Message
      -- parameter must: be a syntactically valid JSON object; and contain at least
      -- a top-level JSON key of "default" with a value that is a string. You can
      -- define other top-level keys that define the message you want to send to a
      -- specific transport protocol (e.g., "http"). For information about sending
      -- different messages for each protocol using the AWS Management Console, go
      -- to Create Different Messages for Each Protocol in the Amazon Simple
      -- Notification Service Getting Started Guide. Valid value: json.
    , piSubject :: Maybe Text
      -- ^ Optional parameter to be used as the "Subject" line when the message is
      -- delivered to email endpoints. This field will also be included, if present,
      -- in the standard JSON messages delivered to other endpoints. Constraints:
      -- Subjects must be ASCII text that begins with a letter, number, or
      -- punctuation mark; must not include line breaks or control characters; and
      -- must be less than 100 characters long.
    , piTargetArn :: Maybe Text
      -- ^ Either TopicArn or EndpointArn, but not both.
    , piTopicArn :: Maybe Text
      -- ^ The topic you want to publish to.
    } deriving (Eq, Show, Generic)

instance ToQuery Publish

instance AWSRequest Publish where
    type Er Publish = SNSError
    type Rs Publish = PublishResponse
    request = getQuery service "Publish"

data PublishResponse = PublishResponse
    { pirsMessageId :: Maybe Text
      -- ^ Unique identifier assigned to the published message. Length Constraint:
      -- Maximum 100 characters.
    } deriving (Eq, Show, Generic)

instance FromXML PublishResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "PublishResponse"
        :| ["PublishResult"]
