{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.RemovePermission
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The RemovePermission action revokes any permissions in the queue policy
-- that matches the specified Label parameter. Only the owner of the queue can
-- remove permissions.
module Network.AWS.SQS.RemovePermission where

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
removePermission :: Text
                 -> Text
                 -> RemovePermission
removePermission p1 p2 = RemovePermission
    { rprLabel = p1
    , rprQueueUrl = p2
    }

data RemovePermission = RemovePermission
    { rprLabel :: !Text
      -- ^ The identification of the permission to remove. This is the label added
      -- with the AddPermission operation.
    , rprQueueUrl :: !Text
      -- ^ The URL of the SQS queue to take action on.
    } deriving (Eq, Show, Generic)

instance ToQuery RemovePermission

instance AWSRequest RemovePermission where
    type Er RemovePermission = SQSError
    type Rs RemovePermission = RemovePermissionResponse
    request = getQuery service "RemovePermission"

data RemovePermissionResponse = RemovePermissionResponse
    deriving (Eq, Show, Generic)

instance FromXML RemovePermissionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot RemovePermissionResponse
