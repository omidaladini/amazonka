{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SES.Monadic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This module is provided for convenience. It offers an alternative to the
-- common idiom of supplying required fields to an operations's smart constructor,
-- using the operation's lenses to modify additional fields, and then sending
-- or paginating the request.
--
-- As an example: using "Network.AWS.SES" with the smart constructor and
-- basic lens syntax, before explicitly calling 'send':
--
-- @
-- import Control.Monad.Trans.AWS
-- import Network.AWS.SES
--
-- send $ (mkOperationName w x)
--      & onLensField1 .~ y
--      & onLensField2 .~ z
-- @
--
-- Versus using "Network.AWS.SES.Monadic" with the 'State' operator variants from
-- "Control.Lens.Setter" such as '.=' to modify any additional request
-- parameters before sending:
--
-- @
-- import Control.Applicative
-- import Network.AWS.SES.Monadic
--
-- operationName w x $ do
--     onLensField1 .= y
--     onLensField2 .= z
--
-- -- Or to void any additional parameters outside of those required using 'Control.Applicative.empty':
-- operationName w x empty
-- @
--
module Network.AWS.SES.Monadic
    (
    -- * DeleteIdentity
    -- $DeleteIdentity
      deleteIdentity
    , deleteIdentityCatch

    -- * DeleteVerifiedEmailAddress
    -- $DeleteVerifiedEmailAddress
    , deleteVerifiedEmailAddress
    , deleteVerifiedEmailAddressCatch

    -- * GetIdentityDkimAttributes
    -- $GetIdentityDkimAttributes
    , getIdentityDkimAttributes
    , getIdentityDkimAttributesCatch

    -- * GetIdentityNotificationAttributes
    -- $GetIdentityNotificationAttributes
    , getIdentityNotificationAttributes
    , getIdentityNotificationAttributesCatch

    -- * GetIdentityVerificationAttributes
    -- $GetIdentityVerificationAttributes
    , getIdentityVerificationAttributes
    , getIdentityVerificationAttributesCatch

    -- * GetSendQuota
    -- $GetSendQuota
    , getSendQuota
    , getSendQuotaCatch

    -- * GetSendStatistics
    -- $GetSendStatistics
    , getSendStatistics
    , getSendStatisticsCatch

    -- * ListIdentities
    -- $ListIdentities
    , listIdentities
    , listIdentitiesCatch

    -- * ListVerifiedEmailAddresses
    -- $ListVerifiedEmailAddresses
    , listVerifiedEmailAddresses
    , listVerifiedEmailAddressesCatch

    -- * SendEmail
    -- $SendEmail
    , sendEmail
    , sendEmailCatch

    -- * SendRawEmail
    -- $SendRawEmail
    , sendRawEmail
    , sendRawEmailCatch

    -- * SetIdentityDkimEnabled
    -- $SetIdentityDkimEnabled
    , setIdentityDkimEnabled
    , setIdentityDkimEnabledCatch

    -- * SetIdentityFeedbackForwardingEnabled
    -- $SetIdentityFeedbackForwardingEnabled
    , setIdentityFeedbackForwardingEnabled
    , setIdentityFeedbackForwardingEnabledCatch

    -- * SetIdentityNotificationTopic
    -- $SetIdentityNotificationTopic
    , setIdentityNotificationTopic
    , setIdentityNotificationTopicCatch

    -- * VerifyDomainDkim
    -- $VerifyDomainDkim
    , verifyDomainDkim
    , verifyDomainDkimCatch

    -- * VerifyDomainIdentity
    -- $VerifyDomainIdentity
    , verifyDomainIdentity
    , verifyDomainIdentityCatch

    -- * VerifyEmailAddress
    -- $VerifyEmailAddress
    , verifyEmailAddress
    , verifyEmailAddressCatch

    -- * VerifyEmailIdentity
    -- $VerifyEmailIdentity
    , verifyEmailIdentity
    , verifyEmailIdentityCatch

    -- * Re-exported
    , module Network.AWS.SES

    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.SES

type ServiceEr = Er SES

-- $DeleteIdentity
-- Deletes the specified identity (email address or domain) from the list of
-- verified identities. This action is throttled at one request per second.
-- POST / HTTP/1.1 Date: Sat, 12 May 2012 05:25:58 GMT Host:
-- email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=w943pl3zIvtszwzZxypi+LsgjzquvhYhnG42S6b2WLo=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 135
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=DeleteIdentity
-- &Identity=domain.com &Timestamp=2012-05-12T05%3A25%3A58.000Z
-- &Version=2010-12-01 d96bd874-9bf2-11e1-8ee7-c98a0037a2b6.
--
-- See: 'Network.AWS.SES.DeleteIdentity'

deleteIdentity :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'diIdentity'
    -> m DeleteIdentityResponse
deleteIdentity p1 s =
    send $ (mkDeleteIdentity p1) &~ s

deleteIdentityCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'diIdentity'
    -> m (Either ServiceEr DeleteIdentityResponse)
deleteIdentityCatch p1 s =
    sendCatch $ (mkDeleteIdentity p1) &~ s

-- $DeleteVerifiedEmailAddress
-- Deletes the specified email address from the list of verified addresses.
-- The DeleteVerifiedEmailAddress action is deprecated as of the May 15, 2012
-- release of Domain Verification. The DeleteIdentity action is now preferred.
-- This action is throttled at one request per second. POST / HTTP/1.1 Date:
-- Thu, 18 Aug 2011 22:20:50 GMT Host: email.us-east-1.amazonaws.com
-- Content-Type: application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=Rxzyd6cQe/YjkV4yoQAZ243OzzNjFgrsclizTKwRIRc=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 142
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=DeleteVerifiedEmailAddress
-- &EmailAddress=user%40example.com &Timestamp=2011-08-18T22%3A20%3A50.000Z
-- 5634af08-c865-11e0-8986-3f99a698f914.
--
-- See: 'Network.AWS.SES.DeleteVerifiedEmailAddress'

deleteVerifiedEmailAddress :: ( MonadCatch m
                              , MonadResource m
                              , MonadError AWS.Error m
                              , MonadReader Env m
                              )
    => Text -- ^ 'dveaEmailAddress'
    -> m DeleteVerifiedEmailAddressResponse
deleteVerifiedEmailAddress p1 s =
    send $ (mkDeleteVerifiedEmailAddress p1) &~ s

deleteVerifiedEmailAddressCatch :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadReader Env m
                                   )
    => Text -- ^ 'dveaEmailAddress'
    -> m (Either ServiceEr DeleteVerifiedEmailAddressResponse)
deleteVerifiedEmailAddressCatch p1 s =
    sendCatch $ (mkDeleteVerifiedEmailAddress p1) &~ s

-- $GetIdentityDkimAttributes
-- Returns the current status of Easy DKIM signing for an entity. For domain
-- name identities, this action also returns the DKIM tokens that are required
-- for Easy DKIM signing, and whether Amazon SES has successfully verified
-- that these tokens have been published. This action takes a list of
-- identities as input and returns the following information for each: Whether
-- Easy DKIM signing is enabled or disabled. A set of DKIM tokens that
-- represent the identity. If the identity is an email address, the tokens
-- represent the domain of that address. Whether Amazon SES has successfully
-- verified the DKIM tokens published in the domain's DNS. This information is
-- only returned for domain name identities, not for email addresses. This
-- action is throttled at one request per second. For more information about
-- creating DNS records using DKIM tokens, go to the Amazon SES Developer
-- Guide. POST / HTTP/1.1 Date: Fri, 29 Jun 2012 22:41:32 GMT Host:
-- email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=MJdhrIAt3c4BRC6jdzueMM+AJLEx17bnIHjZwlSenyk=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 165
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=GetIdentityDkimAttributes
-- &Identities.member.1=example.com &Timestamp=2012-06-29T22%3A41%3A32.000Z
-- &Version=2010-12-01 amazon.com true Success
-- vvjuipp74whm76gqoni7qmwwn4w4qusjiainivf6f
-- 3frqe7jn4obpuxjpwpolz6ipb3k5nvt2nhjpik2oy
-- wrqplteh7oodxnad7hsl4mixg2uavzneazxv5sxi2
-- bb5a105d-c468-11e1-82eb-dff885ccc06a.
--
-- See: 'Network.AWS.SES.GetIdentityDkimAttributes'

getIdentityDkimAttributes :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env m
                             )
    => [Text] -- ^ 'gidaIdentities'
    -> m GetIdentityDkimAttributesResponse
getIdentityDkimAttributes p1 s =
    send $ (mkGetIdentityDkimAttributes p1) &~ s

getIdentityDkimAttributesCatch :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadReader Env m
                                  )
    => [Text] -- ^ 'gidaIdentities'
    -> m (Either ServiceEr GetIdentityDkimAttributesResponse)
getIdentityDkimAttributesCatch p1 s =
    sendCatch $ (mkGetIdentityDkimAttributes p1) &~ s

-- $GetIdentityNotificationAttributes
-- Given a list of verified identities (email addresses and/or domains),
-- returns a structure describing identity notification attributes. This
-- action is throttled at one request per second. For more information about
-- using notifications with Amazon SES, see the Amazon SES Developer Guide.
-- POST / HTTP/1.1 Date: Fri, 15 Jun 2012 20:51:42 GMT Host:
-- email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=ee9aH6tUW5wBPoh01Tz3w4H+z4avrMmvmRYbfORC7OI=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 173
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &Action=GetIdentityNotificationAttributes
-- &Identities.member.1=user%40example.com
-- &Timestamp=2012-06-15T20%3A51%3A42.000Z &Version=2010-12-01
-- user@example.com true arn:aws:sns:us-east-1:123456789012:example
-- arn:aws:sns:us-east-1:123456789012:example
-- arn:aws:sns:us-east-1:123456789012:example
-- e038e509-b72a-11e1-901f-1fbd90e8104f.
--
-- See: 'Network.AWS.SES.GetIdentityNotificationAttributes'

getIdentityNotificationAttributes :: ( MonadCatch m
                                     , MonadResource m
                                     , MonadError AWS.Error m
                                     , MonadReader Env m
                                     )
    => [Text] -- ^ 'ginaIdentities'
    -> m GetIdentityNotificationAttributesResponse
getIdentityNotificationAttributes p1 s =
    send $ (mkGetIdentityNotificationAttributes p1) &~ s

getIdentityNotificationAttributesCatch :: ( MonadCatch m
                                          , MonadResource m
                                          , MonadReader Env m
                                          )
    => [Text] -- ^ 'ginaIdentities'
    -> m (Either ServiceEr GetIdentityNotificationAttributesResponse)
getIdentityNotificationAttributesCatch p1 s =
    sendCatch $ (mkGetIdentityNotificationAttributes p1) &~ s

-- $GetIdentityVerificationAttributes
-- Given a list of identities (email addresses and/or domains), returns the
-- verification status and (for domain identities) the verification token for
-- each identity. This action is throttled at one request per second. POST /
-- HTTP/1.1 Date: Sat, 12 May 2012 05:27:54 GMT Host:
-- email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=3+KQ4VHx991T7Kb41HmFcZJxuHz4/6mf2H5FxY+tuLc=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 203
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &Action=GetIdentityVerificationAttributes
-- &Identities.member.1=user%40domain.com &Identities.member.2=domain.com
-- &Timestamp=2012-05-12T05%3A27%3A54.000Z &Version=2010-12-01 domain.com
-- Pending QTKknzFg2J4ygwa+XvHAxUl1hyHoY0gVfZdfjIedHZ0= user@domain.com
-- Pending 1d0c29f1-9bf3-11e1-8ee7-c98a0037a2b6.
--
-- See: 'Network.AWS.SES.GetIdentityVerificationAttributes'

getIdentityVerificationAttributes :: ( MonadCatch m
                                     , MonadResource m
                                     , MonadError AWS.Error m
                                     , MonadReader Env m
                                     )
    => [Text] -- ^ 'givaIdentities'
    -> m GetIdentityVerificationAttributesResponse
getIdentityVerificationAttributes p1 s =
    send $ (mkGetIdentityVerificationAttributes p1) &~ s

getIdentityVerificationAttributesCatch :: ( MonadCatch m
                                          , MonadResource m
                                          , MonadReader Env m
                                          )
    => [Text] -- ^ 'givaIdentities'
    -> m (Either ServiceEr GetIdentityVerificationAttributesResponse)
getIdentityVerificationAttributesCatch p1 s =
    sendCatch $ (mkGetIdentityVerificationAttributes p1) &~ s

-- $GetSendQuota
-- Returns the user's current sending limits. This action is throttled at one
-- request per second. POST / HTTP/1.1 Date: Thu, 18 Aug 2011 22:22:36 GMT
-- Host: email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=W1YdiNOtf0jN3t7Lv63qhz7UZc3RrcmQpkGbopvnj/Y=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 94
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=GetSendQuota
-- &Timestamp=2011-08-18T22%3A22%3A36.000Z 127.0 200.0 1.0
-- 273021c6-c866-11e0-b926-699e21c3af9e.
--
-- See: 'Network.AWS.SES.GetSendQuota'

getSendQuota :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => State GetSendQuota a
    -> m GetSendQuotaResponse
getSendQuota s =
    send (mkGetSendQuota &~ s)

getSendQuotaCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => State GetSendQuota a
    -> m (Either ServiceEr GetSendQuotaResponse)
getSendQuotaCatch s =
    sendCatch (mkGetSendQuota &~ s)

-- $GetSendStatistics
-- Returns the user's sending statistics. The result is a list of data points,
-- representing the last two weeks of sending activity. Each data point in the
-- list contains statistics for a 15-minute interval. This action is throttled
-- at one request per second. POST / HTTP/1.1 Date: Thu, 18 Aug 2011 22:23:01
-- GMT Host: email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=kwuk4eraA9HSfHySflgDKR6xK0JXjATIE7Uu5/FB4x4=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 99
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=GetSendStatistics
-- &Timestamp=2011-08-18T22%3A23%3A01.000Z 8 2011-08-03T19:23:00Z 0 0 0 7
-- 2011-08-03T06:53:00Z 0 0 0 . . . . c2b66ee5-c866-11e0-b17f-cddb0ab334db.
--
-- See: 'Network.AWS.SES.GetSendStatistics'

getSendStatistics :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
    => State GetSendStatistics a
    -> m GetSendStatisticsResponse
getSendStatistics s =
    send (mkGetSendStatistics &~ s)

getSendStatisticsCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env m
                          )
    => State GetSendStatistics a
    -> m (Either ServiceEr GetSendStatisticsResponse)
getSendStatisticsCatch s =
    sendCatch (mkGetSendStatistics &~ s)

-- $ListIdentities
-- Returns a list containing all of the identities (email addresses and
-- domains) for a specific AWS Account, regardless of verification status.
-- This action is throttled at one request per second. POST / HTTP/1.1 Date:
-- Sat, 12 May 2012 05:18:45 GMT Host: email.us-east-1.amazonaws.com
-- Content-Type: application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=OruiFNV26DCZicLDaQmULHGbjbU8MbC/c5aIo/MMIuM=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 115
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=ListIdentities
-- &Timestamp=2012-05-12T05%3A18%3A45.000Z& Version=2010-12-01 example.com
-- user@example.com cacecf23-9bf1-11e1-9279-0100e8cf109a.
--
-- See: 'Network.AWS.SES.ListIdentities'

listIdentities :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env (ResumableSource m)
                  )
    => State ListIdentities a
    -> ResumableSource m ListIdentitiesResponse
listIdentities s =
    paginate (mkListIdentities &~ s)

listIdentitiesCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env (ResumableSource m)
                       )
    => State ListIdentities a
    -> ResumableSource m (Either ServiceEr ListIdentitiesResponse)
listIdentitiesCatch s =
    paginateCatch (mkListIdentities &~ s)

-- $ListVerifiedEmailAddresses
-- Returns a list containing all of the email addresses that have been
-- verified. The ListVerifiedEmailAddresses action is deprecated as of the May
-- 15, 2012 release of Domain Verification. The ListIdentities action is now
-- preferred. This action is throttled at one request per second. POST /
-- HTTP/1.1 Date: Thu, 18 Aug 2011 22:05:09 GMT Host:
-- email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=II0+vvDKGMv71vToBwzR6vZ1hxe/VUE8tWEFUNTUqgE=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 108
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=ListVerifiedEmailAddresses
-- &Timestamp=2011-08-18T22%3A05%3A09.000Z% example@amazon.com
-- 3dd50e97-c865-11e0-b235-099eb63d928d.
--
-- See: 'Network.AWS.SES.ListVerifiedEmailAddresses'

listVerifiedEmailAddresses :: ( MonadCatch m
                              , MonadResource m
                              , MonadError AWS.Error m
                              , MonadReader Env m
                              )
    => State ListVerifiedEmailAddresses a
    -> m ListVerifiedEmailAddressesResponse
listVerifiedEmailAddresses s =
    send (mkListVerifiedEmailAddresses &~ s)

listVerifiedEmailAddressesCatch :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadReader Env m
                                   )
    => State ListVerifiedEmailAddresses a
    -> m (Either ServiceEr ListVerifiedEmailAddressesResponse)
listVerifiedEmailAddressesCatch s =
    sendCatch (mkListVerifiedEmailAddresses &~ s)

-- $SendEmail
-- Composes an email message based on input data, and then immediately queues
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
--
-- See: 'Network.AWS.SES.SendEmail'

sendEmail :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env m
             )
    => Text -- ^ 'seSource'
    -> Destination -- ^ 'seDestination'
    -> Message -- ^ 'seMessage'
    -> m SendEmailResponse
sendEmail p1 p2 p3 s =
    send $ (mkSendEmail p1 p2 p3) &~ s

sendEmailCatch :: ( MonadCatch m
                  , MonadResource m
                  , MonadReader Env m
                  )
    => Text -- ^ 'seSource'
    -> Destination -- ^ 'seDestination'
    -> Message -- ^ 'seMessage'
    -> m (Either ServiceEr SendEmailResponse)
sendEmailCatch p1 p2 p3 s =
    sendCatch $ (mkSendEmail p1 p2 p3) &~ s

-- $SendRawEmail
-- Sends an email message, with header and content specified by the client.
-- The SendRawEmail action is useful for sending multipart MIME emails. The
-- raw text of the message must comply with Internet email standards;
-- otherwise, the message cannot be sent. You can only send email from
-- verified email addresses and domains. If you have not requested production
-- access to Amazon SES, you must also verify every recipient email address
-- except for the recipients provided by the Amazon SES mailbox simulator. For
-- more information, go to the Amazon SES Developer Guide. The total size of
-- the message cannot exceed 10 MB. This includes any attachments that are
-- part of the message. Amazon SES has a limit on the total number of
-- recipients per message: The combined number of To:, CC: and BCC: email
-- addresses cannot exceed 50. If you need to send an email message to a
-- larger audience, you can divide your recipient list into groups of 50 or
-- fewer, and then call Amazon SES repeatedly to send the message to each
-- group. The To:, CC:, and BCC: headers in the raw message can contain a
-- group list. Note that each recipient in a group list counts towards the
-- 50-recipient limit. For every message that you send, the total number of
-- recipients (To:, CC: and BCC:) is counted against your sending quota - the
-- maximum number of emails you can send in a 24-hour period. For information
-- about your sending quota, go to the Amazon SES Developer Guide. POST /
-- HTTP/1.1 Date: Wed, 17 Aug 2011 00:21:38 GMT Host:
-- email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=uN0lHIf14tmMBzwnkHzaWBLrBFvJAvyXCsfSYAvwLuc=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 230
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=SendRawEmail
-- &RawMessage.Data=U3ViamVjdDogRXhhbXBsZQpGcm9tOiBleGFtcGxlQGFtYXpvbi5jb20KVG8
-- 
-- 6IGV4YW1wbGVAYW1h%0Aem9uLmNvbQpDb250ZW50LVR5cGU6IG11bHRpcGFydC9hbHRlcm5hdGl2
-- 
-- ZTsgYm91bmRhcnk9MDAx%0ANmU2OGY5ZDkyOWNiMDk2MDRhYWE4MzA0MgoKLS0wMDE2ZTY4ZjlkO
-- 
-- TI5Y2IwOTYwNGFhYTgzMDQy%0ACkNvbnRlbnQtVHlwZTogdGV4dC9wbGFpbjsgY2hhcnNldD1JU0
-- 
-- 8tODg1OS0xCgpCb2R5LgoKLS0w%0AMDE2ZTY4ZjlkOTI5Y2IwOTYwNGFhYTgzMDQyCkNvbnRlbnQ
-- 
-- tVHlwZTogdGV4dC9odG1sOyBjaGFy%0Ac2V0PUlTTy04ODU5LTEKCkJvZHkuPGJyPgoKLS0wMDE2
-- ZTY4ZjlkOTI5Y2IwOTYwNGFhYTgzMDQy%0ALS0%3D%0A
-- &Timestamp=2011-08-17T00%3A21%3A38.000Z
-- 00000131d51d6b36-1d4f9293-0aee-4503-b573-9ae4e70e9e38-000000
-- e0abcdfa-c866-11e0-b6d0-273d09173b49.
--
-- See: 'Network.AWS.SES.SendRawEmail'

sendRawEmail :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                )
    => RawMessage -- ^ 'sreRawMessage'
    -> m SendRawEmailResponse
sendRawEmail p3 s =
    send $ (mkSendRawEmail p3) &~ s

sendRawEmailCatch :: ( MonadCatch m
                     , MonadResource m
                     , MonadReader Env m
                     )
    => RawMessage -- ^ 'sreRawMessage'
    -> m (Either ServiceEr SendRawEmailResponse)
sendRawEmailCatch p3 s =
    sendCatch $ (mkSendRawEmail p3) &~ s

-- $SetIdentityDkimEnabled
-- Enables or disables Easy DKIM signing of email sent from an identity: If
-- Easy DKIM signing is enabled for a domain name identity (e.g.,
-- example.com), then Amazon SES will DKIM-sign all email sent by addresses
-- under that domain name (e.g., user@example.com). If Easy DKIM signing is
-- enabled for an email address, then Amazon SES will DKIM-sign all email sent
-- by that email address. For email addresses (e.g., user@example.com), you
-- can only enable Easy DKIM signing if the corresponding domain (e.g.,
-- example.com) has been set up for Easy DKIM using the AWS Console or the
-- VerifyDomainDkim action. This action is throttled at one request per
-- second. For more information about Easy DKIM signing, go to the Amazon SES
-- Developer Guide. POST / HTTP/1.1 Date: Fri, 29 Jun 2012 22:42:08 GMT Host:
-- email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=u/hDNhYm87AV7LAPzouTBz6HJxUEuE5k96sLzYHjR24=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 168
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=SetIdentityDkimEnabled
-- &DkimEnabled=true&Identity=user%40example.com
-- &Timestamp=2012-06-29T22%3A42%3A08.000Z &Version=2010-12-01
-- 7aa61362-c469-11e1-aee5-6bbb4608fbcc.
--
-- See: 'Network.AWS.SES.SetIdentityDkimEnabled'

setIdentityDkimEnabled :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          )
    => Text -- ^ 'sideIdentity'
    -> Bool -- ^ 'sideDkimEnabled'
    -> m SetIdentityDkimEnabledResponse
setIdentityDkimEnabled p1 p2 s =
    send $ (mkSetIdentityDkimEnabled p1 p2) &~ s

setIdentityDkimEnabledCatch :: ( MonadCatch m
                               , MonadResource m
                               , MonadReader Env m
                               )
    => Text -- ^ 'sideIdentity'
    -> Bool -- ^ 'sideDkimEnabled'
    -> m (Either ServiceEr SetIdentityDkimEnabledResponse)
setIdentityDkimEnabledCatch p1 p2 s =
    sendCatch $ (mkSetIdentityDkimEnabled p1 p2) &~ s

-- $SetIdentityFeedbackForwardingEnabled
-- Given an identity (email address or domain), enables or disables whether
-- Amazon SES forwards bounce and complaint notifications as email. Feedback
-- forwarding can only be disabled when Amazon Simple Notification Service
-- (Amazon SNS) topics are specified for both bounces and complaints. Feedback
-- forwarding does not apply to delivery notifications. Delivery notifications
-- are only available through Amazon SNS. This action is throttled at one
-- request per second. For more information about using notifications with
-- Amazon SES, see the Amazon SES Developer Guide. POST / HTTP/1.1 Date: Fri,
-- 15 Jun 2012 20:31:21 GMT Host: email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=juNpmD6UJaN+r7gcLa2ZNZpO3AmF1ZfOkD6PgxgNhRA=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 188
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &Action=SetIdentityFeedbackForwardingEnabled &ForwardingEnabled=true
-- &Identity=user%40example.com &Timestamp=2012-06-15T20%3A31%3A21.000Z
-- &Version=2010-12-01 299f4af4-b72a-11e1-901f-1fbd90e8104f.
--
-- See: 'Network.AWS.SES.SetIdentityFeedbackForwardingEnabled'

setIdentityFeedbackForwardingEnabled :: ( MonadCatch m
                                        , MonadResource m
                                        , MonadError AWS.Error m
                                        , MonadReader Env m
                                        )
    => Text -- ^ 'siffeIdentity'
    -> Bool -- ^ 'siffeForwardingEnabled'
    -> m SetIdentityFeedbackForwardingEnabledResponse
setIdentityFeedbackForwardingEnabled p1 p2 s =
    send $ (mkSetIdentityFeedbackForwardingEnabled p1 p2) &~ s

setIdentityFeedbackForwardingEnabledCatch :: ( MonadCatch m
                                             , MonadResource m
                                             , MonadReader Env m
                                             )
    => Text -- ^ 'siffeIdentity'
    -> Bool -- ^ 'siffeForwardingEnabled'
    -> m (Either ServiceEr SetIdentityFeedbackForwardingEnabledResponse)
setIdentityFeedbackForwardingEnabledCatch p1 p2 s =
    sendCatch $ (mkSetIdentityFeedbackForwardingEnabled p1 p2) &~ s

-- $SetIdentityNotificationTopic
-- Given an identity (email address or domain), sets the Amazon Simple
-- Notification Service (Amazon SNS) topic to which Amazon SES will publish
-- bounce, complaint, and/or delivery notifications for emails sent with that
-- identity as the Source. Unless feedback forwarding is enabled, you must
-- specify Amazon SNS topics for bounce and complaint notifications. For more
-- information, see SetIdentityFeedbackForwardingEnabled. This action is
-- throttled at one request per second. For more information about feedback
-- notification, see the Amazon SES Developer Guide. POST / HTTP/1.1 Date:
-- Sat, 12 May 2012 05:27:54 GMT Host: email.us-east-1.amazonaws.com
-- Content-Type: application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=3+KQ4VHx991T7Kb41HmFcZJxuHz4/6mf2H5FxY+tuLc=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 203
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=SetIdentityNotificationTopic
-- &Identity=user@example.com
-- &SnsTopic=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3Aexample
-- &NotificationType=Bounce
-- &Timestamp=2012-05-12T05%3A27%3A54.000Z&Version=2010-12-01
-- 299f4af4-b72a-11e1-901f-1fbd90e8104f.
--
-- See: 'Network.AWS.SES.SetIdentityNotificationTopic'

setIdentityNotificationTopic :: ( MonadCatch m
                                , MonadResource m
                                , MonadError AWS.Error m
                                , MonadReader Env m
                                )
    => Text -- ^ 'sintIdentity'
    -> NotificationType -- ^ 'sintNotificationType'
    -> m SetIdentityNotificationTopicResponse
setIdentityNotificationTopic p1 p2 s =
    send $ (mkSetIdentityNotificationTopic p1 p2) &~ s

setIdentityNotificationTopicCatch :: ( MonadCatch m
                                     , MonadResource m
                                     , MonadReader Env m
                                     )
    => Text -- ^ 'sintIdentity'
    -> NotificationType -- ^ 'sintNotificationType'
    -> m (Either ServiceEr SetIdentityNotificationTopicResponse)
setIdentityNotificationTopicCatch p1 p2 s =
    sendCatch $ (mkSetIdentityNotificationTopic p1 p2) &~ s

-- $VerifyDomainDkim
-- Returns a set of DKIM tokens for a domain. DKIM tokens are character
-- strings that represent your domain's identity. Using these tokens, you will
-- need to create DNS CNAME records that point to DKIM public keys hosted by
-- Amazon SES. Amazon Web Services will eventually detect that you have
-- updated your DNS records; this detection process may take up to 72 hours.
-- Upon successful detection, Amazon SES will be able to DKIM-sign email
-- originating from that domain. This action is throttled at one request per
-- second. To enable or disable Easy DKIM signing for a domain, use the
-- SetIdentityDkimEnabled action. For more information about creating DNS
-- records using DKIM tokens, go to the Amazon SES Developer Guide. POST /
-- HTTP/1.1 Date: Fri, 29 Jun 2012 22:43:30 GMT Host:
-- email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=roXhd+JhEjeBBo5tSERhrptRHSw4XHz6Ra4BXyHIduk=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 136
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=VerifyDomainDkim
-- &Domain=example.com &Timestamp=2012-06-29T22%3A43%3A30.000Z
-- &Version=2010-12-01 vvjuipp74whm76gqoni7qmwwn4w4qusjiainivf6sf
-- 3frqe7jn4obpuxjpwpolz6ipb3k5nvt2nhjpik2oy
-- wrqplteh7oodxnad7hsl4mixg2uavzneazxv5sxi2
-- 9662c15b-c469-11e1-99d1-797d6ecd6414.
--
-- See: 'Network.AWS.SES.VerifyDomainDkim'

verifyDomainDkim :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'vddDomain'
    -> m VerifyDomainDkimResponse
verifyDomainDkim p1 s =
    send $ (mkVerifyDomainDkim p1) &~ s

verifyDomainDkimCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'vddDomain'
    -> m (Either ServiceEr VerifyDomainDkimResponse)
verifyDomainDkimCatch p1 s =
    sendCatch $ (mkVerifyDomainDkim p1) &~ s

-- $VerifyDomainIdentity
-- Verifies a domain. This action is throttled at one request per second. POST
-- / HTTP/1.1 Date: Sat, 12 May 2012 05:24:02 GMT Host:
-- email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=Wr+6RCfV+QgjLki2dtIrlecMK9+RrsDaTG5uWneDAu8=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 139
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=VerifyDomainIdentity
-- &Domain=domain.com &Timestamp=2012-05-12T05%3A24%3A02.000Z
-- &Version=2010-12-01 QTKknzFg2J4ygwa+XvHAxUl1hyHoY0gVfZdfjIedHZ0=
-- 94f6368e-9bf2-11e1-8ee7-c98a0037a2b6.
--
-- See: 'Network.AWS.SES.VerifyDomainIdentity'

verifyDomainIdentity :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env m
                        )
    => Text -- ^ 'vdiDomain'
    -> m VerifyDomainIdentityResponse
verifyDomainIdentity p1 s =
    send $ (mkVerifyDomainIdentity p1) &~ s

verifyDomainIdentityCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env m
                             )
    => Text -- ^ 'vdiDomain'
    -> m (Either ServiceEr VerifyDomainIdentityResponse)
verifyDomainIdentityCatch p1 s =
    sendCatch $ (mkVerifyDomainIdentity p1) &~ s

-- $VerifyEmailAddress
-- Verifies an email address. This action causes a confirmation email message
-- to be sent to the specified address. The VerifyEmailAddress action is
-- deprecated as of the May 15, 2012 release of Domain Verification. The
-- VerifyEmailIdentity action is now preferred. This action is throttled at
-- one request per second. POST / HTTP/1.1 Date: Thu, 18 Aug 2011 22:28:27 GMT
-- Host: email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=o9NK68jraFg5BnaTQiQhpxj2x1dGONOEFHHgsM6o5as=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 132
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=VerifyEmailAddress
-- &EmailAddress=user%40example.com &Timestamp=2011-08-18T22%3A28%3A27.000Z
-- 8edd7eb2-c864-11e0-9f8f-3da8fc215a7e.
--
-- See: 'Network.AWS.SES.VerifyEmailAddress'

verifyEmailAddress :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => Text -- ^ 'veaEmailAddress'
    -> m VerifyEmailAddressResponse
verifyEmailAddress p1 s =
    send $ (mkVerifyEmailAddress p1) &~ s

verifyEmailAddressCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => Text -- ^ 'veaEmailAddress'
    -> m (Either ServiceEr VerifyEmailAddressResponse)
verifyEmailAddressCatch p1 s =
    sendCatch $ (mkVerifyEmailAddress p1) &~ s

-- $VerifyEmailIdentity
-- Verifies an email address. This action causes a confirmation email message
-- to be sent to the specified address. This action is throttled at one
-- request per second. POST / HTTP/1.1 Date: Sat, 12 May 2012 05:21:58 GMT
-- Host: email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=hQJj2pxypqJHQgU/BW1EZGUiNIYGhkQDf7tI6UgQ2qw=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 151
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=VerifyEmailIdentity
-- &EmailAddress=user%40domain.com &Timestamp=2012-05-12T05%3A21%3A58.000Z
-- &Version=2010-12-01 47e0ef1a-9bf2-11e1-9279-0100e8cf109a.
--
-- See: 'Network.AWS.SES.VerifyEmailIdentity'

verifyEmailIdentity :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => Text -- ^ 'veiEmailAddress'
    -> m VerifyEmailIdentityResponse
verifyEmailIdentity p1 s =
    send $ (mkVerifyEmailIdentity p1) &~ s

verifyEmailIdentityCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => Text -- ^ 'veiEmailAddress'
    -> m (Either ServiceEr VerifyEmailIdentityResponse)
verifyEmailIdentityCatch p1 s =
    sendCatch $ (mkVerifyEmailIdentity p1) &~ s