{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.AddPermission
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The AddPermission action adds a statement to a topic's access control
-- policy, granting access for the specified AWS accounts to the specified
-- actions. http://sns.us-east-1.amazonaws.com/
-- ?TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Test
-- &ActionName.member.1=Publish &ActionName.member.2=GetTopicAttributes
-- &Label=NewPermission &AWSAccountId.member.1=987654321000
-- &AWSAccountId.member.2=876543210000 &Action=AddPermission
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2010-03-31T12%3A00%3A00.000Z &AWSAccessKeyId=(AWS Access Key ID)
-- &Signature=k%2FAU%2FKp13pjndwJ7rr1sZszy6MZMlOhRBCHx1ZaZFiw%3D
-- 6a213e4e-33a8-11df-9540-99d0768312d3.
module Network.AWS.SNS.AddPermission where

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
addPermission :: [Text]
              -> [Text]
              -> Text
              -> Text
              -> AddPermission
addPermission p1 p2 p3 p4 = AddPermission
    { apiAWSAccountId = p1
    , apiActionName = p2
    , apiLabel = p3
    , apiTopicArn = p4
    }

data AddPermission = AddPermission
    { apiAWSAccountId :: [Text]
      -- ^ The AWS account IDs of the users (principals) who will be given access to
      -- the specified actions. The users must have AWS accounts, but do not need to
      -- be signed up for this service. account identification, see Your AWS
      -- Identifiers in the &service; Developer Guide.-->.
    , apiActionName :: [Text]
      -- ^ The action you want to allow for the specified principal(s). Valid values:
      -- any Amazon SNS action name.
    , apiLabel :: !Text
      -- ^ A unique identifier for the new policy statement.
    , apiTopicArn :: !Text
      -- ^ The ARN of the topic whose access control policy you wish to modify.
    } deriving (Eq, Show, Generic)

instance ToQuery AddPermission

instance AWSRequest AddPermission where
    type Er AddPermission = SNSError
    type Rs AddPermission = AddPermissionResponse
    request = getQuery service "AddPermission"

data AddPermissionResponse = AddPermissionResponse
    deriving (Eq, Show, Generic)

instance FromXML AddPermissionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot AddPermissionResponse
