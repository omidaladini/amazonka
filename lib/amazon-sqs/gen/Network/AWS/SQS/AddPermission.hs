{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.AddPermission
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The AddPermission action adds a permission to a queue for a specific
-- principal. This allows for sharing access to the queue. When you create a
-- queue, you have full control access rights for the queue. Only you (as
-- owner of the queue) can grant or deny permissions to the queue. For more
-- information about these permissions, see Shared Queues in the Amazon SQS
-- Developer Guide. AddPermission writes an SQS-generated policy. If you want
-- to write your own policy, use SetQueueAttributes to upload your policy. For
-- more information about writing your own policy, see Appendix: The Access
-- Policy Language in the Amazon SQS Developer Guide.
module Network.AWS.SQS.AddPermission where

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

import Network.AWS.SQS.Service
import Network.AWS.SQS.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
addPermission :: [Text]
              -> [Text]
              -> Text
              -> Text
              -> AddPermission
addPermission p1 p2 p3 p4 = AddPermission
    { aprAWSAccountIds = p1
    , aprActions = p2
    , aprLabel = p3
    , aprQueueUrl = p4
    }

data AddPermission = AddPermission
    { aprAWSAccountIds :: [Text]
      -- ^ The AWS account number of the principal who will be given permission. The
      -- principal must have an AWS account, but does not need to be signed up for
      -- Amazon SQS.
    , aprActions :: [Text]
      -- ^ The action the client wants to allow for the specified principal.
    , aprLabel :: !Text
      -- ^ The unique identification of the permission you're setting (e.g.,
      -- AliceSendMessage). Constraints: Maximum 80 characters; alphanumeric
      -- characters, hyphens (-), and underscores (_) are allowed.
    , aprQueueUrl :: !Text
      -- ^ The URL of the SQS queue to take action on.
    } deriving (Eq, Show, Generic)

instance ToQuery AddPermission

instance AWSRequest AddPermission where
    type Er AddPermission = SQSError
    type Rs AddPermission = AddPermissionResponse
    request = getQuery service "AddPermission"

data AddPermissionResponse = AddPermissionResponse
    deriving (Eq, Show, Generic)

instance FromXML AddPermissionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot AddPermissionResponse
