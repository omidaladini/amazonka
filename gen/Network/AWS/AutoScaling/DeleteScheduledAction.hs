{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DeleteScheduledAction
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a scheduled action previously created using the
-- PutScheduledUpdateGroupAction.
module Network.AWS.AutoScaling.DeleteScheduledAction where

import Network.AWS.Core
import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteScheduledAction :: ResourceName
                      -- ^ The name of the action you want to delete.
                      -> DeleteScheduledAction
deleteScheduledAction p1 = DeleteScheduledAction
    { dsatScheduledActionName = p1
    , dsatAutoScalingGroupName = Nothing
    }

data DeleteScheduledAction = DeleteScheduledAction
    { dsatAutoScalingGroupName :: Maybe ResourceName
      -- ^ The name of the Auto Scaling group.
    , dsatScheduledActionName :: !ResourceName
      -- ^ The name of the action you want to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteScheduledAction

instance AWSRequest DeleteScheduledAction where
    type Er DeleteScheduledAction = AutoScalingError
    type Rs DeleteScheduledAction = DeleteScheduledActionResponse
    request  = postQuery service "DeleteScheduledAction"
    response = responseXML

data DeleteScheduledActionResponse = DeleteScheduledActionResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteScheduledActionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteScheduledActionResponse"
