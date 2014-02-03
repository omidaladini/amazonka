-- Module      : Network.AWS.SES
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.SES
    (
    -- * Operations
    -- ** GetSendQuota
      module Network.AWS.SES.GetSendQuota
    -- ** GetIdentityNotificationAttributes
    , module Network.AWS.SES.GetIdentityNotificationAttributes
    -- ** SetIdentityDkimEnabled
    , module Network.AWS.SES.SetIdentityDkimEnabled
    -- ** SetIdentityFeedbackForwardingEnabled
    , module Network.AWS.SES.SetIdentityFeedbackForwardingEnabled
    -- ** GetIdentityVerificationAttributes
    , module Network.AWS.SES.GetIdentityVerificationAttributes
    -- ** VerifyDomainIdentity
    , module Network.AWS.SES.VerifyDomainIdentity
    -- ** GetIdentityDkimAttributes
    , module Network.AWS.SES.GetIdentityDkimAttributes
    -- ** VerifyDomainDkim
    , module Network.AWS.SES.VerifyDomainDkim
    -- ** SendRawEmail
    , module Network.AWS.SES.SendRawEmail
    -- ** GetSendStatistics
    , module Network.AWS.SES.GetSendStatistics
    -- ** DeleteIdentity
    , module Network.AWS.SES.DeleteIdentity
    -- ** ListIdentities
    , module Network.AWS.SES.ListIdentities
    -- ** VerifyEmailIdentity
    , module Network.AWS.SES.VerifyEmailIdentity
    -- ** VerifyEmailAddress
    , module Network.AWS.SES.VerifyEmailAddress
    -- ** DeleteVerifiedEmailAddress
    , module Network.AWS.SES.DeleteVerifiedEmailAddress
    -- ** ListVerifiedEmailAddresses
    , module Network.AWS.SES.ListVerifiedEmailAddresses
    -- ** SetIdentityNotificationTopic
    , module Network.AWS.SES.SetIdentityNotificationTopic
    -- ** SendEmail
    , module Network.AWS.SES.SendEmail

    -- * Types
    -- ** SendDataPoint
    , SendDataPoint (..)
    -- ** RawMessage
    , RawMessage (..)
    -- ** Message
    , Message (..)
    -- ** IdentityVerificationAttributes
    , IdentityVerificationAttributes (..)
    -- ** IdentityNotificationAttributes
    , IdentityNotificationAttributes (..)
    -- ** IdentityDkimAttributes
    , IdentityDkimAttributes (..)
    -- ** Destination
    , Destination (..)
    -- ** Content
    , Content (..)
    -- ** Body
    , Body (..)
    -- ** VerificationStatus
    , VerificationStatus (..)
    -- ** NotificationType
    , NotificationType (..)
    -- ** IdentityType
    , IdentityType (..)

    -- * Errors
    , SESError (..)
    ) where

import Network.AWS.SES.Service
import Network.AWS.SES.Types

import Network.AWS.SES.GetSendQuota
import Network.AWS.SES.GetIdentityNotificationAttributes
import Network.AWS.SES.SetIdentityDkimEnabled
import Network.AWS.SES.SetIdentityFeedbackForwardingEnabled
import Network.AWS.SES.GetIdentityVerificationAttributes
import Network.AWS.SES.VerifyDomainIdentity
import Network.AWS.SES.GetIdentityDkimAttributes
import Network.AWS.SES.VerifyDomainDkim
import Network.AWS.SES.SendRawEmail
import Network.AWS.SES.GetSendStatistics
import Network.AWS.SES.DeleteIdentity
import Network.AWS.SES.ListIdentities
import Network.AWS.SES.VerifyEmailIdentity
import Network.AWS.SES.VerifyEmailAddress
import Network.AWS.SES.DeleteVerifiedEmailAddress
import Network.AWS.SES.ListVerifiedEmailAddresses
import Network.AWS.SES.SetIdentityNotificationTopic
import Network.AWS.SES.SendEmail
