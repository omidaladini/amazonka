{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.SendEmail
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Composes an email message based on input data, and then immediately queues
-- the message for sending. You can only send email from verified email
-- addresses and domains. If you have not requested production access to
-- Amazon SES, you must also verify every recipient email address except for
-- the recipients provided by the Amazon SES mailbox simulator. For more
-- information, go to the Amazon SES Developer Guide. The total size of the
-- message cannot exceed 10 MB. Amazon SES has a limit on the total number of
-- recipients per message: The combined number of To:, CC: and BCC: email
-- addresses cannot exceed 50. If you need to send an email message to a
-- larger audience, you can divide your recipient list into groups of 50 or
-- fewer, and then call Amazon SES repeatedly to send the message to each
-- group. For every message that you send, the total number of recipients
-- (To:, CC: and BCC:) is counted against your sending quota - the maximum
-- number of emails you can send in a 24-hour period. For information about
-- your sending quota, go to the Amazon SES Developer Guide. POST / HTTP/1.1
-- Date: Thu, 18 Aug 2011 22:25:27 GMT Host: email.us-east-1.amazonaws.com
-- Content-Type: application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=yXx/wM1bESLuDErJ6HpZg9JK8Gjau7EUe4FWEfmhodo=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 230
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=SendEmail
-- &Destination.ToAddresses.member.1=allan%40example.com
-- &Message.Body.Text.Data=body
-- &Message.Subject.Data=Example&Source=user%40example.com
-- &Timestamp=2011-08-18T22%3A25%3A27.000Z
-- 00000131d51d2292-159ad6eb-077c-46e6-ad09-ae7c05925ed4-000000
-- d5964849-c866-11e0-9beb-01a62d68c57f.
module Network.AWS.SES.SendEmail where

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

-- | Convenience method utilising default fields where applicable.
sendEmail :: Destination
          -> Message
          -> Text
          -> AWS (Either SESError SendEmailResponse)
sendEmail p1 p2 p3 = undefined $ SendEmail
    { serDestination = p1
    , serMessage = p2
    , serSource = p3
    , serReplyToAddresses = []
    , serReturnPath = Nothing
    }

data SendEmail = SendEmail
    { serDestination :: Destination
      -- ^ The destination for this email, composed of To:, CC:, and BCC: fields.
    , serMessage :: Message
      -- ^ The message to be sent.
    , serReplyToAddresses :: [Text]
      -- ^ The reply-to email address(es) for the message. If the recipient replies to
      -- the message, each reply-to address will receive the reply.
    , serReturnPath :: Maybe Text
      -- ^ The email address to which bounce notifications are to be forwarded. If the
      -- message cannot be delivered to the recipient, then an error message will be
      -- returned from the recipient's ISP; this message will then be forwarded to
      -- the email address specified by the ReturnPath parameter.
    , serSource :: !Text
      -- ^ The identity's email address. By default, the string must be 7-bit ASCII.
      -- If the text must contain any other characters, then you must use MIME
      -- encoded-word syntax (RFC 2047) instead of a literal string. MIME
      -- encoded-word syntax uses the following form:
      -- =?charset?encoding?encoded-text?=. For more information, see RFC 2047.
    } deriving (Eq, Show, Generic)

instance ToQuery SendEmail

instance AWSRequest SendEmail where
    type Er SendEmail = SESError
    type Rs SendEmail = SendEmailResponse
    request = getQuery service "SendEmail"

data SendEmailResponse = SendEmailResponse
    { serrsMessageId :: !Text
      -- ^ The unique message identifier returned from the SendEmail action.
    } deriving (Eq, Show, Generic)

instance FromXML SendEmailResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "SendEmailResponse"
        :| ["SendEmailResult"]
