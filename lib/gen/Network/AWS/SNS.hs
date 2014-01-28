-- Module      : Network.AWS.SNS
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.SNS
    (
    -- * Operations
    -- ** DeleteEndpoint
      module Network.AWS.SNS.DeleteEndpoint
    -- ** RemovePermission
    , module Network.AWS.SNS.RemovePermission
    -- ** DeleteTopic
    , module Network.AWS.SNS.DeleteTopic
    -- ** ListTopics
    , module Network.AWS.SNS.ListTopics
    -- ** CreatePlatformEndpoint
    , module Network.AWS.SNS.CreatePlatformEndpoint
    -- ** SetPlatformApplicationAttributes
    , module Network.AWS.SNS.SetPlatformApplicationAttributes
    -- ** ListSubscriptionsByTopic
    , module Network.AWS.SNS.ListSubscriptionsByTopic
    -- ** GetTopicAttributes
    , module Network.AWS.SNS.GetTopicAttributes
    -- ** CreatePlatformApplication
    , module Network.AWS.SNS.CreatePlatformApplication
    -- ** GetPlatformApplicationAttributes
    , module Network.AWS.SNS.GetPlatformApplicationAttributes
    -- ** ListEndpointsByPlatformApplication
    , module Network.AWS.SNS.ListEndpointsByPlatformApplication
    -- ** SetTopicAttributes
    , module Network.AWS.SNS.SetTopicAttributes
    -- ** DeletePlatformApplication
    , module Network.AWS.SNS.DeletePlatformApplication
    -- ** ListPlatformApplications
    , module Network.AWS.SNS.ListPlatformApplications
    -- ** AddPermission
    , module Network.AWS.SNS.AddPermission
    -- ** GetEndpointAttributes
    , module Network.AWS.SNS.GetEndpointAttributes
    -- ** ListSubscriptions
    , module Network.AWS.SNS.ListSubscriptions
    -- ** GetSubscriptionAttributes
    , module Network.AWS.SNS.GetSubscriptionAttributes
    -- ** CreateTopic
    , module Network.AWS.SNS.CreateTopic
    -- ** Subscribe
    , module Network.AWS.SNS.Subscribe
    -- ** Unsubscribe
    , module Network.AWS.SNS.Unsubscribe
    -- ** SetEndpointAttributes
    , module Network.AWS.SNS.SetEndpointAttributes
    -- ** SetSubscriptionAttributes
    , module Network.AWS.SNS.SetSubscriptionAttributes
    -- ** ConfirmSubscription
    , module Network.AWS.SNS.ConfirmSubscription
    -- ** Publish
    , module Network.AWS.SNS.Publish

    -- * Types
    -- ** Topic
    , Topic (..)
    -- ** Subscription
    , Subscription (..)
    -- ** PlatformApplication
    , PlatformApplication (..)
    -- ** Endpoint
    , Endpoint (..)

    -- * Errors
    , SNSError (..)
    ) where

import Network.AWS.SNS.Service
import Network.AWS.SNS.Types

import Network.AWS.SNS.DeleteEndpoint
import Network.AWS.SNS.RemovePermission
import Network.AWS.SNS.DeleteTopic
import Network.AWS.SNS.ListTopics
import Network.AWS.SNS.CreatePlatformEndpoint
import Network.AWS.SNS.SetPlatformApplicationAttributes
import Network.AWS.SNS.ListSubscriptionsByTopic
import Network.AWS.SNS.GetTopicAttributes
import Network.AWS.SNS.CreatePlatformApplication
import Network.AWS.SNS.GetPlatformApplicationAttributes
import Network.AWS.SNS.ListEndpointsByPlatformApplication
import Network.AWS.SNS.SetTopicAttributes
import Network.AWS.SNS.DeletePlatformApplication
import Network.AWS.SNS.ListPlatformApplications
import Network.AWS.SNS.AddPermission
import Network.AWS.SNS.GetEndpointAttributes
import Network.AWS.SNS.ListSubscriptions
import Network.AWS.SNS.GetSubscriptionAttributes
import Network.AWS.SNS.CreateTopic
import Network.AWS.SNS.Subscribe
import Network.AWS.SNS.Unsubscribe
import Network.AWS.SNS.SetEndpointAttributes
import Network.AWS.SNS.SetSubscriptionAttributes
import Network.AWS.SNS.ConfirmSubscription
import Network.AWS.SNS.Publish
