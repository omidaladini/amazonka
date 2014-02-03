{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.Service
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.SQS.Service where

import Data.Default
import Data.Tagged
import GHC.Generics               (Generic)
import Network.AWS.Internal
import Network.AWS.Internal.Types (Service(..))
import Text.XML.Generic

-- | Currently supported version (@2012-11-05@) of the @Amazon Simple Queue Service@ service.
service :: Service
service = Service Global v4 "sqs" "2012-11-05"

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlInherit   = True
    , xmlNamespace = Just "http://queue.amazonaws.com/doc/2012-11-05/"
    }

data SQSError
    = BatchEntryIdsNotDistinct
    | BatchRequestTooLong
    | EmptyBatchRequest
    | InvalidAttributeName
    | InvalidBatchEntryId
    | InvalidIdFormat
    | InvalidMessageContents
    | MessageNotInflight
    | OverLimit
    | QueueDeletedRecently
    | QueueDoesNotExist
    | QueueNameExists
    | ReceiptHandleIsInvalid
    | TooManyEntriesInBatchRequest
      deriving (Eq, Show, Generic)

instance FromXML SQSError where
    fromXMLOptions = xmlOptions
