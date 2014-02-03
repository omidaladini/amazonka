{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.SetIdentityFeedbackForwardingEnabled
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Given an identity (email address or domain), enables or disables whether
-- Amazon SES forwards feedback notifications as email. Feedback forwarding
-- may only be disabled when both complaint and bounce topics are set. This
-- action is throttled at one request per second. For more information about
-- feedback notification, see the Amazon SES Developer Guide. POST / HTTP/1.1
-- Date: Fri, 15 Jun 2012 20:31:21 GMT Host: email.us-east-1.amazonaws.com
-- Content-Type: application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=juNpmD6UJaN+r7gcLa2ZNZpO3AmF1ZfOkD6PgxgNhRA=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 188
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &Action=SetIdentityFeedbackForwardingEnabled &ForwardingEnabled=true
-- &Identity=user%40example.com &Timestamp=2012-06-15T20%3A31%3A21.000Z
-- &Version=2010-12-01 299f4af4-b72a-11e1-901f-1fbd90e8104f.
module Network.AWS.SES.SetIdentityFeedbackForwardingEnabled where

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
setIdentityFeedbackForwardingEnabled :: Bool
                                     -> Text
                                     -> SetIdentityFeedbackForwardingEnabled
setIdentityFeedbackForwardingEnabled p1 p2 = SetIdentityFeedbackForwardingEnabled
    { sifferForwardingEnabled = p1
    , sifferIdentity = p2
    }

data SetIdentityFeedbackForwardingEnabled = SetIdentityFeedbackForwardingEnabled
    { sifferForwardingEnabled :: !Bool
      -- ^ Sets whether Amazon SES will forward feedback notifications as email. true
      -- specifies that Amazon SES will forward feedback notifications as email, in
      -- addition to any Amazon SNS topic publishing otherwise specified. false
      -- specifies that Amazon SES will publish feedback notifications only through
      -- Amazon SNS. This value can only be set to false when topics are specified
      -- for both Bounce and Complaint topic types.
    , sifferIdentity :: !Text
      -- ^ The identity for which to set feedback notification forwarding. Examples:
      -- user@example.com, example.com.
    } deriving (Eq, Show, Generic)

instance ToQuery SetIdentityFeedbackForwardingEnabled

instance AWSRequest SetIdentityFeedbackForwardingEnabled where
    type Er SetIdentityFeedbackForwardingEnabled = SESError
    type Rs SetIdentityFeedbackForwardingEnabled = SetIdentityFeedbackForwardingEnabledResponse
    request = getQuery service "SetIdentityFeedbackForwardingEnabled"

data SetIdentityFeedbackForwardingEnabledResponse = SetIdentityFeedbackForwardingEnabledResponse
    deriving (Eq, Show, Generic)

instance FromXML SetIdentityFeedbackForwardingEnabledResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot SetIdentityFeedbackForwardingEnabledResponse
