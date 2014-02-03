{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.DeleteTopic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeleteTopic action deletes a topic and all its subscriptions. Deleting
-- a topic might prevent some messages previously sent to the topic from being
-- delivered to subscribers. This action is idempotent, so deleting a topic
-- that does not exist does not result in an error.
-- http://sns.us-east-1.amazonaws.com/
-- &TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic
-- &Action=DeleteTopic &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2010-03-31T12%3A00%3A00.000Z &AWSAccessKeyId=(AWS Access Key Id)
-- &Signature=mQA3nJI%2BcmAIY7r8HCArGElSqPX5JG4UGzF4yo0RygE%3D
-- f3aa9ac9-3c3d-11df-8235-9dab105e9c32.
module Network.AWS.SNS.DeleteTopic where

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
deleteTopic :: Text
            -> DeleteTopic
deleteTopic p1 = DeleteTopic
    { dtiTopicArn = p1
    }

data DeleteTopic = DeleteTopic
    { dtiTopicArn :: !Text
      -- ^ The ARN of the topic you want to delete.
      -- http://sns.us-east-1.amazonaws.com/
      -- ?TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic
      -- &Action=DeleteTopic &SignatureVersion=2 &SignatureMethod=HmacSHA256
      -- &Timestamp=2010-03-31T12%3A00%3A00.000Z &AWSAccessKeyId=(AWS Access Key ID)
      -- &Signature=DjHBa%2BbYCKQAzctOPnLP7MbHnrHT3%2FK3kFEZjwcf9%2FU%3D
      -- fba800b9-3765-11df-8cf3-c58c53254dfb.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteTopic

instance AWSRequest DeleteTopic where
    type Er DeleteTopic = SNSError
    type Rs DeleteTopic = DeleteTopicResponse
    request = getQuery service "DeleteTopic"

data DeleteTopicResponse = DeleteTopicResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteTopicResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot DeleteTopicResponse
