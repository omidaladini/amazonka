{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SQS.V2012_11_05.DeleteQueue
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the queue specified by the queue URL, regardless of whether the
-- queue is empty. If the specified queue does not exist, Amazon SQS returns a
-- successful response. Use DeleteQueue with care; once you delete your queue,
-- any messages in the queue are no longer available. When you delete a queue,
-- the deletion process takes up to 60 seconds. Requests you send involving
-- that queue during the 60 seconds might succeed. For example, a SendMessage
-- request might succeed, but after the 60 seconds, the queue and that message
-- you sent no longer exist. Also, when you delete a queue, you must wait at
-- least 60 seconds before creating a queue with the same name. We reserve the
-- right to delete queues that have had no activity for more than 30 days. For
-- more information, see How Amazon SQS Queues Work in the Amazon SQS
-- Developer Guide. The following example Query request deletes the specified
-- queue. http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- ?Action=DeleteQueue &Version=2009-02-01 &SignatureMethod=HmacSHA256
-- &Expires=2009-04-18T22%3A52%3A43PST &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &SignatureVersion=2 &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- 6fde8d1e-52cd-4581-8cd9-c512f4c64223.
module Network.AWS.SQS.V2012_11_05.DeleteQueue where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.SQS.V2012_11_05.Types
import Network.AWS.Prelude

data DeleteQueue = DeleteQueue
    { _dqrQueueUrl :: Text
      -- ^ The URL of the Amazon SQS queue to take action on.
    } deriving (Show, Generic)

makeLenses ''DeleteQueue

instance ToQuery DeleteQueue where
    toQuery = genericToQuery def

data DeleteQueueResponse = DeleteQueueResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteQueueResponse

instance AWSRequest DeleteQueue where
    type Sv DeleteQueue = SQS
    type Rs DeleteQueue = DeleteQueueResponse

    request = post "DeleteQueue"
    response _ = nullaryResponse DeleteQueueResponse