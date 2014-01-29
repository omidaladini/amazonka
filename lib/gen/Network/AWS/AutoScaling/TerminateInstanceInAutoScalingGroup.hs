{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Terminates the specified instance. Optionally, the desired group size can
-- be adjusted. This call simply registers a termination request. The
-- termination of the instance cannot happen immediately.
module Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup where

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

-- | Convenience method utilising default fields where applicable.
terminateInstanceInAutoScalingGroup :: Text
                                    -> Bool
                                    -> AWS (Either AutoScalingError TerminateInstanceInAutoScalingGroupResponse)
terminateInstanceInAutoScalingGroup p1 p2 = undefined $ TerminateInstanceInAutoScalingGroup
    { tiiasgtInstanceId = p1
    , tiiasgtShouldDecrementDesiredCapacity = p2
    }

data TerminateInstanceInAutoScalingGroup = TerminateInstanceInAutoScalingGroup
    { tiiasgtInstanceId :: !Text
      -- ^ The ID of the Amazon EC2 instance to be terminated.
    , tiiasgtShouldDecrementDesiredCapacity :: !Bool
      -- ^ Specifies whether (true) or not (false) terminating this instance should
      -- also decrement the size of the AutoScalingGroup.
    } deriving (Eq, Show, Generic)

instance ToQuery TerminateInstanceInAutoScalingGroup

instance AWSRequest TerminateInstanceInAutoScalingGroup where
    type Er TerminateInstanceInAutoScalingGroup = AutoScalingError
    type Rs TerminateInstanceInAutoScalingGroup = TerminateInstanceInAutoScalingGroupResponse
    request = getQuery service "TerminateInstanceInAutoScalingGroup"

data TerminateInstanceInAutoScalingGroupResponse = TerminateInstanceInAutoScalingGroupResponse
    { tiiasgtrsActivity :: Maybe Activity
      -- ^ A scaling Activity.
    } deriving (Eq, Show, Generic)

instance FromXML TerminateInstanceInAutoScalingGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "TerminateInstanceInAutoScalingGroupResponse"
        :| ["TerminateInstanceInAutoScalingGroupResult"]
