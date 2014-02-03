-- Module      : Network.AWS.SQS
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.SQS
    (
    -- * Operations
    -- ** GetQueueUrl
      module Network.AWS.SQS.GetQueueUrl
    -- ** ChangeMessageVisibilityBatch
    , module Network.AWS.SQS.ChangeMessageVisibilityBatch
    -- ** SendMessage
    , module Network.AWS.SQS.SendMessage
    -- ** RemovePermission
    , module Network.AWS.SQS.RemovePermission
    -- ** GetQueueAttributes
    , module Network.AWS.SQS.GetQueueAttributes
    -- ** ListQueues
    , module Network.AWS.SQS.ListQueues
    -- ** ReceiveMessage
    , module Network.AWS.SQS.ReceiveMessage
    -- ** DeleteQueue
    , module Network.AWS.SQS.DeleteQueue
    -- ** DeleteMessageBatch
    , module Network.AWS.SQS.DeleteMessageBatch
    -- ** SetQueueAttributes
    , module Network.AWS.SQS.SetQueueAttributes
    -- ** AddPermission
    , module Network.AWS.SQS.AddPermission
    -- ** DeleteMessage
    , module Network.AWS.SQS.DeleteMessage
    -- ** CreateQueue
    , module Network.AWS.SQS.CreateQueue
    -- ** SendMessageBatch
    , module Network.AWS.SQS.SendMessageBatch
    -- ** ChangeMessageVisibility
    , module Network.AWS.SQS.ChangeMessageVisibility

    -- * Types
    -- ** SendMessageBatchResultEntry
    , SendMessageBatchResultEntry (..)
    -- ** SendMessageBatchRequestEntry
    , SendMessageBatchRequestEntry (..)
    -- ** Message
    , Message (..)
    -- ** DeleteMessageBatchResultEntry
    , DeleteMessageBatchResultEntry (..)
    -- ** DeleteMessageBatchRequestEntry
    , DeleteMessageBatchRequestEntry (..)
    -- ** ChangeMessageVisibilityBatchResultEntry
    , ChangeMessageVisibilityBatchResultEntry (..)
    -- ** ChangeMessageVisibilityBatchRequestEntry
    , ChangeMessageVisibilityBatchRequestEntry (..)
    -- ** BatchResultErrorEntry
    , BatchResultErrorEntry (..)
    -- ** QueueAttributeName
    , QueueAttributeName (..)

    -- * Errors
    , SQSError (..)
    ) where

import Network.AWS.SQS.Service
import Network.AWS.SQS.Types

import Network.AWS.SQS.GetQueueUrl
import Network.AWS.SQS.ChangeMessageVisibilityBatch
import Network.AWS.SQS.SendMessage
import Network.AWS.SQS.RemovePermission
import Network.AWS.SQS.GetQueueAttributes
import Network.AWS.SQS.ListQueues
import Network.AWS.SQS.ReceiveMessage
import Network.AWS.SQS.DeleteQueue
import Network.AWS.SQS.DeleteMessageBatch
import Network.AWS.SQS.SetQueueAttributes
import Network.AWS.SQS.AddPermission
import Network.AWS.SQS.DeleteMessage
import Network.AWS.SQS.CreateQueue
import Network.AWS.SQS.SendMessageBatch
import Network.AWS.SQS.ChangeMessageVisibility
