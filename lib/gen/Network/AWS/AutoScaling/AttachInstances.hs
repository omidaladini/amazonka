{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.AttachInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Attaches one or more Amazon EC2 instances to an existing Auto Scaling
-- group. After the instance(s) is attached, it becomes a part of the Auto
-- Scaling group. For more information, see Attach Amazon EC2 Instance(s) to
-- Your Existing Auto Scaling Group in the Auto Scaling Developer Guide.
module Network.AWS.AutoScaling.AttachInstances where

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

import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
attachInstances :: ResourceName
                -> AttachInstances
attachInstances p1 = undefined $ AttachInstances
    { aiqAutoScalingGroupName = p1
    , aiqInstanceIds = []
    }

data AttachInstances = AttachInstances
    { aiqAutoScalingGroupName :: !ResourceName
      -- ^ The name of the Auto Scaling group to which to attach the specified
      -- instance(s).
    , aiqInstanceIds :: [Text]
      -- ^ One or more IDs of the Amazon EC2 instances to attach to the specified Auto
      -- Scaling group. You must specify at least one instance ID.
    } deriving (Eq, Show, Generic)

instance ToQuery AttachInstances

instance AWSRequest AttachInstances where
    type Er AttachInstances = AutoScalingError
    type Rs AttachInstances = AttachInstancesResponse
    request = getQuery service "AttachInstances"

data AttachInstancesResponse = AttachInstancesResponse
    deriving (Eq, Show, Generic)

instance FromXML AttachInstancesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "AttachInstancesResponse"
        :| ["AttachInstancesResult"]
