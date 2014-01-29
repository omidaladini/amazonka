{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.ChangeMessageVisibilityBatch
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This is a batch version of ChangeMessageVisibility. It takes multiple
-- receipt handles and performs the operation on each of the them. The result
-- of the operation on each message is reported individually in the response.
module Network.AWS.SQS.ChangeMessageVisibilityBatch where

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
changeMessageVisibilityBatch :: [ChangeMessageVisibilityBatchRequestEntry]
                             -> Text
                             -> ChangeMessageVisibilityBatch
changeMessageVisibilityBatch p1 p2 = undefined $ ChangeMessageVisibilityBatch
    { cmvbrEntries = p1
    , cmvbrQueueUrl = p2
    }

data ChangeMessageVisibilityBatch = ChangeMessageVisibilityBatch
    { cmvbrEntries :: [ChangeMessageVisibilityBatchRequestEntry]
      -- ^ A list of receipt handles of the messages for which the visibility timeout
      -- must be changed.
    , cmvbrQueueUrl :: !Text
      -- ^ The URL of the SQS queue to take action on.
    } deriving (Eq, Show, Generic)

instance ToQuery ChangeMessageVisibilityBatch

instance AWSRequest ChangeMessageVisibilityBatch where
    type Er ChangeMessageVisibilityBatch = SQSError
    type Rs ChangeMessageVisibilityBatch = ChangeMessageVisibilityBatchResponse
    request = getQuery service "ChangeMessageVisibilityBatch"

data ChangeMessageVisibilityBatchResponse = ChangeMessageVisibilityBatchResponse
    { cmvbrrsFailed :: [BatchResultErrorEntry]
      -- ^ A list of BatchResultErrorEntrys.
    , cmvbrrsSuccessful :: [ChangeMessageVisibilityBatchResultEntry]
      -- ^ A list of ChangeMessageVisibilityBatchResultEntrys.
    } deriving (Eq, Show, Generic)

instance FromXML ChangeMessageVisibilityBatchResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ChangeMessageVisibilityBatchResponse"
        :| ["ChangeMessageVisibilityBatchResult"]
