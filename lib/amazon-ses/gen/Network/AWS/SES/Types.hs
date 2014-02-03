{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.SES.Types where

import Data.ByteString      (ByteString)
import Data.Hashable
import Data.Monoid
import Data.List.NonEmpty
import Data.Text            (Text)
import Data.HashMap.Strict  (HashMap)
import Data.Time
import GHC.Generics         (Generic)
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.SES.Service

-- | Represents sending statistics data. Each SendDataPoint contains statistics
-- for a 15-minute period of sending activity.
data SendDataPoint = SendDataPoint
    { sdpBounces :: Maybe Integer
      -- ^ Number of emails that have bounced.
    , sdpComplaints :: Maybe Integer
      -- ^ Number of unwanted emails that were rejected by recipients.
    , sdpDeliveryAttempts :: Maybe Integer
      -- ^ Number of emails that have been enqueued for sending.
    , sdpRejects :: Maybe Integer
      -- ^ Number of emails rejected by Amazon SES.
    , sdpTimestamp :: Maybe UTCTime
      -- ^ Time of the data point.
    } deriving (Eq, Show, Generic)

instance ToQuery SendDataPoint

instance FromXML SendDataPoint where
    fromXMLOptions = xmlOptions

instance ToXML SendDataPoint where
    toXMLOptions = xmlOptions

-- | The raw text of the message. The client is responsible for ensuring the
-- following: Message must contain a header and a body, separated by a blank
-- line. All required header fields must be present. Each part of a multipart
-- MIME message must be formatted properly. MIME content types must be among
-- those supported by Amazon SES. For more information, go to the Amazon SES
-- Developer Guide. Content must be base64-encoded, if MIME requires it.
newtype RawMessage = RawMessage
    { rmData :: Blob
      -- ^ The raw data of the message. The client must ensure that the message format
      -- complies with Internet email standards regarding email header fields, MIME
      -- types, MIME encoding, and base64 encoding (if necessary). For more
      -- information, go to the Amazon SES Developer Guide.
    } deriving (Eq, Show, Generic)

instance ToQuery RawMessage

instance FromXML RawMessage where
    fromXMLOptions = xmlOptions

instance ToXML RawMessage where
    toXMLOptions = xmlOptions

-- | The message to be sent.
data Message = Message
    { mBody :: Body
      -- ^ The message body.
    , mSubject :: Content
      -- ^ The subject of the message: A short summary of the content, which will
      -- appear in the recipient's inbox.
    } deriving (Eq, Show, Generic)

instance ToQuery Message

instance FromXML Message where
    fromXMLOptions = xmlOptions

instance ToXML Message where
    toXMLOptions = xmlOptions

-- | Represents the verification attributes of a single identity.
data IdentityVerificationAttributes = IdentityVerificationAttributes
    { ivaVerificationStatus :: !VerificationStatus
      -- ^ The verification status of the identity: "Pending", "Success", "Failed", or
      -- "TemporaryFailure".
    , ivaVerificationToken :: Maybe Text
      -- ^ The verification token for a domain identity. Null for email address
      -- identities.
    } deriving (Eq, Show, Generic)

instance ToQuery IdentityVerificationAttributes

instance FromXML IdentityVerificationAttributes where
    fromXMLOptions = xmlOptions

instance ToXML IdentityVerificationAttributes where
    toXMLOptions = xmlOptions

-- | Represents the notification attributes of an identity, including whether a
-- bounce or complaint topic are set, and whether feedback forwarding is
-- enabled.
data IdentityNotificationAttributes = IdentityNotificationAttributes
    { inaBounceTopic :: !Text
      -- ^ The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
      -- (SNS) topic where Amazon SES will publish bounce notifications.
    , inaComplaintTopic :: !Text
      -- ^ The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
      -- (SNS) topic where Amazon SES will publish complaint notifications.
    , inaForwardingEnabled :: !Bool
      -- ^ Describes whether Amazon SES will forward feedback as email. true indicates
      -- that Amazon SES will forward feedback as email, while false indicates that
      -- feedback will be published only to the specified Bounce and Complaint
      -- topics.
    } deriving (Eq, Show, Generic)

instance ToQuery IdentityNotificationAttributes

instance FromXML IdentityNotificationAttributes where
    fromXMLOptions = xmlOptions

instance ToXML IdentityNotificationAttributes where
    toXMLOptions = xmlOptions

-- | Represents the DKIM attributes of a verified email address or a domain.
data IdentityDkimAttributes = IdentityDkimAttributes
    { idaDkimEnabled :: !Bool
      -- ^ True if DKIM signing is enabled for email sent from the identity; false
      -- otherwise.
    , idaDkimTokens :: [Text]
      -- ^ A set of character strings that represent the domain's identity. Using
      -- these tokens, you will need to create DNS CNAME records that point to DKIM
      -- public keys hosted by Amazon SES. Amazon Web Services will eventually
      -- detect that you have updated your DNS records; this detection process may
      -- take up to 72 hours. Upon successful detection, Amazon SES will be able to
      -- DKIM-sign email originating from that domain. (This only applies to domain
      -- identities, not email address identities.) For more information about
      -- creating DNS records using DKIM tokens, go to the Amazon SES Developer
      -- Guide.
    , idaDkimVerificationStatus :: !VerificationStatus
      -- ^ Describes whether Amazon SES has successfully verified the DKIM DNS records
      -- (tokens) published in the domain name's DNS. (This only applies to domain
      -- identities, not email address identities.).
    } deriving (Eq, Show, Generic)

instance ToQuery IdentityDkimAttributes

instance FromXML IdentityDkimAttributes where
    fromXMLOptions = xmlOptions

instance ToXML IdentityDkimAttributes where
    toXMLOptions = xmlOptions

-- | The destination for this email, composed of To:, CC:, and BCC: fields.
data Destination = Destination
    { dBccAddresses :: [Text]
      -- ^ The BCC: field(s) of the message.
    , dCcAddresses :: [Text]
      -- ^ The CC: field(s) of the message.
    , dToAddresses :: [Text]
      -- ^ The To: field(s) of the message.
    } deriving (Eq, Show, Generic)

instance ToQuery Destination

instance FromXML Destination where
    fromXMLOptions = xmlOptions

instance ToXML Destination where
    toXMLOptions = xmlOptions

-- | The subject of the message: A short summary of the content, which will
-- appear in the recipient's inbox.
data Content = Content
    { cCharset :: Maybe Text
      -- ^ The character set of the content.
    , cData :: !Text
      -- ^ The textual data of the content.
    } deriving (Eq, Show, Generic)

instance ToQuery Content

instance FromXML Content where
    fromXMLOptions = xmlOptions

instance ToXML Content where
    toXMLOptions = xmlOptions

-- | The message body.
data Body = Body
    { bHtml :: Maybe Content
      -- ^ The content of the message, in HTML format. Use this for email clients that
      -- can process HTML. You can include clickable links, formatted text, and much
      -- more in an HTML message.
    , bText :: Maybe Content
      -- ^ The content of the message, in text format. Use this for text-based email
      -- clients, or clients on high-latency networks (such as mobile devices).
    } deriving (Eq, Show, Generic)

instance ToQuery Body

instance FromXML Body where
    fromXMLOptions = xmlOptions

instance ToXML Body where
    toXMLOptions = xmlOptions

-- | The verification status of the identity: "Pending", "Success", "Failed", or
-- "TemporaryFailure".
data VerificationStatus
    = VerificationStatusFailed
    | VerificationStatusNotStarted
    | VerificationStatusPending
    | VerificationStatusSuccess
    | VerificationStatusTemporaryFailure
      deriving (Eq, Ord, Generic)

instance Hashable VerificationStatus

instance FromText VerificationStatus where
    fromText "Failed" = Right VerificationStatusFailed
    fromText "NotStarted" = Right VerificationStatusNotStarted
    fromText "Pending" = Right VerificationStatusPending
    fromText "Success" = Right VerificationStatusSuccess
    fromText "TemporaryFailure" = Right VerificationStatusTemporaryFailure
    fromText e = fromTextFail $ "Unrecognised VerificationStatus: " <> e

instance Read VerificationStatus where
    readsPrec _ = fromTextRead

instance ToText VerificationStatus where
    toText VerificationStatusFailed = "Failed"
    toText VerificationStatusNotStarted = "NotStarted"
    toText VerificationStatusPending = "Pending"
    toText VerificationStatusSuccess = "Success"
    toText VerificationStatusTemporaryFailure = "TemporaryFailure"

instance Show VerificationStatus where
    show = toTextShow

instance ToQuery VerificationStatus where
    toQuery = toTextQuery

instance FromXML VerificationStatus where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML VerificationStatus where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The type of feedback notifications that will be published to the specified
-- topic.
data NotificationType
    = NotificationTypeBounce
    | NotificationTypeComplaint
      deriving (Eq, Ord, Generic)

instance Hashable NotificationType

instance FromText NotificationType where
    fromText "Bounce" = Right NotificationTypeBounce
    fromText "Complaint" = Right NotificationTypeComplaint
    fromText e = fromTextFail $ "Unrecognised NotificationType: " <> e

instance Read NotificationType where
    readsPrec _ = fromTextRead

instance ToText NotificationType where
    toText NotificationTypeBounce = "Bounce"
    toText NotificationTypeComplaint = "Complaint"

instance Show NotificationType where
    show = toTextShow

instance ToQuery NotificationType where
    toQuery = toTextQuery

instance FromXML NotificationType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML NotificationType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The type of the identities to list. Possible values are "EmailAddress" and
-- "Domain". If this parameter is omitted, then all identities will be listed.
data IdentityType
    = IdentityTypeDomain
    | IdentityTypeEmailAddress
      deriving (Eq, Ord, Generic)

instance Hashable IdentityType

instance FromText IdentityType where
    fromText "Domain" = Right IdentityTypeDomain
    fromText "EmailAddress" = Right IdentityTypeEmailAddress
    fromText e = fromTextFail $ "Unrecognised IdentityType: " <> e

instance Read IdentityType where
    readsPrec _ = fromTextRead

instance ToText IdentityType where
    toText IdentityTypeDomain = "Domain"
    toText IdentityTypeEmailAddress = "EmailAddress"

instance Show IdentityType where
    show = toTextShow

instance ToQuery IdentityType where
    toQuery = toTextQuery

instance FromXML IdentityType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML IdentityType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML
