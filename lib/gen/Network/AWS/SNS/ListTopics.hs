{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.ListTopics
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ListTopics action returns a list of the requester's topics. Each call
-- returns a limited list of topics, up to 100. If there are more topics, a
-- NextToken is also returned. Use the NextToken parameter in a new ListTopics
-- call to get further results. http://sns.us-east-1.amazonaws.com/
-- ?Action=ListTopics &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2010-03-31T12%3A00%3A00.000Z &AWSAccessKeyId=(AWS Access Key ID)
-- &Signature=tPg1qKNTNVPydnL3Yx5Fqm2O9GxCr9vh3EF5r9%2F5%2BJs%3D
-- arn:aws:sns:us-east-1:123456789012:My-Topic
-- 3f1478c7-33a9-11df-9540-99d0768312d3.
module Network.AWS.SNS.ListTopics where

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
listTopics :: ListTopics
listTopics = ListTopics
    { ltiNextToken = Nothing
    }

data ListTopics = ListTopics
    { ltiNextToken :: Maybe Text
      -- ^ Token returned by the previous ListTopics request.
    } deriving (Eq, Show, Generic)

instance ToQuery ListTopics

instance AWSRequest ListTopics where
    type Er ListTopics = SNSError
    type Rs ListTopics = ListTopicsResponse
    request = getQuery service "ListTopics"

instance AWSPager ListTopics where
    next rq rs
        | Just x <- ltirsNextToken rs = Just $ rq { ltiNextToken = Just x }
        | otherwise = Nothing

data ListTopicsResponse = ListTopicsResponse
    { ltirsNextToken :: Maybe Text
      -- ^ Token to pass along to the next ListTopics request. This element is
      -- returned if there are additional topics to retrieve.
    , ltirsTopics :: [Topic]
      -- ^ A list of topic ARNs.
    } deriving (Eq, Show, Generic)

instance FromXML ListTopicsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ListTopicsResponse"
        :| ["ListTopicsResult"]
