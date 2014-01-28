{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.CreateTopic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateTopic action creates a topic to which notifications can be
-- published. Users can create at most 100 topics. For more information, see
-- http://aws.amazon.com/sns. This action is idempotent, so if the requester
-- already owns a topic with the specified name, that topic's ARN is returned
-- without creating a new topic. http://sns.us-east-1.amazonaws.com/
-- ?Name=My-Topic &Action=CreateTopic &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2010-03-31T12%3A00%3A00.000Z
-- &AWSAccessKeyId=(AWS Access Key ID)
-- &Signature=gfzIF53exFVdpSNb8AiwN3Lv%2FNYXh6S%2Br3yySK70oX4%3D
-- arn:aws:sns:us-east-1:123456789012:My-Topic
-- a8dec8b3-33a4-11df-8963-01868b7c937a.
module Network.AWS.SNS.CreateTopic where

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

data CreateTopic = CreateTopic
    { ctiName :: !Text
      -- ^ The name of the topic you want to create. Constraints: Topic names must be
      -- made up of only uppercase and lowercase ASCII letters, numbers,
      -- underscores, and hyphens, and must be between 1 and 256 characters long.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateTopic

instance AWSRequest CreateTopic where
    type Er CreateTopic = SNSError
    type Rs CreateTopic = CreateTopicResponse
    request = getQuery service "CreateTopic"

data CreateTopicResponse = CreateTopicResponse
    { ctirsTopicArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) assigned to the created topic.
    } deriving (Eq, Show, Generic)

instance FromXML CreateTopicResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateTopicResponse"
        :| ["CreateTopicResult"]
