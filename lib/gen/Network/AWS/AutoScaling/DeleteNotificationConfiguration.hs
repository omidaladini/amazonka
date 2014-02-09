{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DeleteNotificationConfiguration
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes notifications created by PutNotificationConfiguration.
module Network.AWS.AutoScaling.DeleteNotificationConfiguration where

import Network.AWS.Core
import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

data DeleteNotificationConfiguration = DeleteNotificationConfiguration
    { dnctAutoScalingGroupName :: !ResourceName
      -- ^ The name of the Auto Scaling group.
    , dnctTopicARN :: !ResourceName
      -- ^ The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
      -- (SNS) topic.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteNotificationConfiguration

instance AWSRequest DeleteNotificationConfiguration where
    type Er DeleteNotificationConfiguration = AutoScalingError
    type Rs DeleteNotificationConfiguration = DeleteNotificationConfigurationResponse
    request  = postQuery service "DeleteNotificationConfiguration"
    response = responseXML

data DeleteNotificationConfigurationResponse = DeleteNotificationConfigurationResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteNotificationConfigurationResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteNotificationConfigurationResponse"
