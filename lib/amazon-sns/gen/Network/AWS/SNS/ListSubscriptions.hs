{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.ListSubscriptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ListSubscriptions action returns a list of the requester's
-- subscriptions. Each call returns a limited list of subscriptions, up to
-- 100. If there are more subscriptions, a NextToken is also returned. Use the
-- NextToken parameter in a new ListSubscriptions call to get further results.
-- http://sns.us-east-1.amazonaws.com/ &Action=ListSubscriptions
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2010-03-31T12%3A00%3A00.000Z &AWSAccessKeyId=(AWS Access Key ID)
-- &Signature=SZmBxEPqfs9R7xxhSt6C1b7PnOEvg%2BSVyyMYJfLRFCA%3D
-- arn:aws:sns:us-east-1:698519295917:My-Topic email
-- arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca
-- 123456789012 example@amazon.com 384ac68d-3775-11df-8963-01868b7c937a.
module Network.AWS.SNS.ListSubscriptions where

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
listSubscriptions :: ListSubscriptions
listSubscriptions = ListSubscriptions
    { lsiNextToken = Nothing
    }

data ListSubscriptions = ListSubscriptions
    { lsiNextToken :: Maybe Text
      -- ^ Token returned by the previous ListSubscriptions request.
    } deriving (Eq, Show, Generic)

instance ToQuery ListSubscriptions

instance AWSRequest ListSubscriptions where
    type Er ListSubscriptions = SNSError
    type Rs ListSubscriptions = ListSubscriptionsResponse
    request = getQuery service "ListSubscriptions"

instance AWSPager ListSubscriptions where
    next rq rs
        | Just x <- lsirsNextToken rs = Just $ rq { lsiNextToken = Just x }
        | otherwise = Nothing

data ListSubscriptionsResponse = ListSubscriptionsResponse
    { lsirsNextToken :: Maybe Text
      -- ^ Token to pass along to the next ListSubscriptions request. This element is
      -- returned if there are more subscriptions to retrieve.
    , lsirsSubscriptions :: [Subscription]
      -- ^ A list of subscriptions.
    } deriving (Eq, Show, Generic)

instance FromXML ListSubscriptionsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ListSubscriptionsResponse"
        :| ["ListSubscriptionsResult"]
