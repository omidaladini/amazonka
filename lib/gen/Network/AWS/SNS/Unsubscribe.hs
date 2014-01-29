{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.Unsubscribe
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The Unsubscribe action deletes a subscription. If the subscription requires
-- authentication for deletion, only the owner of the subscription or the
-- topic's owner can unsubscribe, and an AWS signature is required. If the
-- Unsubscribe call does not require authentication and the requester is not
-- the subscription owner, a final cancellation message is delivered to the
-- endpoint, so that the endpoint owner can easily resubscribe to the topic if
-- the Unsubscribe request was unintended. http://sns.us-east-1.amazonaws.com/
-- ?SubscriptionArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic%3A80289ba6-0fd4-4079-afb4-ce8c8260f0ca
-- &Action=Unsubscribe &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2010-03-31T12%3A00%3A00.000Z &AWSAccessKeyId=(AWS Access Key ID)
-- &Signature=e8IwhPzuWeMvPDVrN7jUVxasd3Wv2LuO8x6rE23VCv8%3D
-- 18e0ac39-3776-11df-84c0-b93cc1666b84.
module Network.AWS.SNS.Unsubscribe where

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
unsubscribe :: Text
            -> Unsubscribe
unsubscribe p1 = undefined $ Unsubscribe
    { uiSubscriptionArn = p1
    }

data Unsubscribe = Unsubscribe
    { uiSubscriptionArn :: !Text
      -- ^ The ARN of the subscription to be deleted.
    } deriving (Eq, Show, Generic)

instance ToQuery Unsubscribe

instance AWSRequest Unsubscribe where
    type Er Unsubscribe = SNSError
    type Rs Unsubscribe = UnsubscribeResponse
    request = getQuery service "Unsubscribe"

data UnsubscribeResponse = UnsubscribeResponse
    deriving (Eq, Show, Generic)

instance FromXML UnsubscribeResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "UnsubscribeResponse"
        :| ["UnsubscribeResult"]
