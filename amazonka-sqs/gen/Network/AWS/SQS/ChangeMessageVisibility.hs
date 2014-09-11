{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.ChangeMessageVisibility
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Changes the visibility timeout of a specified message in a queue to a new
-- value. The maximum allowed timeout value you can set the value to is 12
-- hours. This means you can't extend the timeout of a message in an existing
-- queue to more than a total visibility timeout of 12 hours. (For more
-- information visibility timeout, see Visibility Timeout in the Amazon SQS
-- Developer Guide.) For example, let's say you have a message and its default
-- message visibility timeout is 30 minutes. You could call
-- ChangeMessageVisiblity with a value of two hours and the effective timeout
-- would be two hours and 30 minutes. When that time comes near you could
-- again extend the time out by calling ChangeMessageVisiblity, but this time
-- the maximum allowed timeout would be 9 hours and 30 minutes. There is a
-- 120,000 limit for the number of inflight messages per queue. Messages are
-- inflight after they have been received from the queue by a consuming
-- component, but have not yet been deleted from the queue. If you reach the
-- 120,000 limit, you will receive an OverLimit error message from Amazon SQS.
-- To help avoid reaching the limit, you should delete the messages from the
-- queue after they have been processed. You can also increase the number of
-- queues you use to process the messages. If you attempt to set the
-- VisibilityTimeout to an amount more than the maximum time left, Amazon SQS
-- returns an error. It will not automatically recalculate and increase the
-- timeout to the maximum time remaining. Unlike with a queue, when you change
-- the visibility timeout for a specific message, that timeout value is
-- applied immediately but is not saved in memory for that message. If you
-- don't delete a message after it is received, the visibility timeout for the
-- message the next time it is received reverts to the original timeout value,
-- not the value you set with the ChangeMessageVisibility action. The
-- following example Query request changes the visibility timeout for a
-- message to 60 seconds.
-- http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- ?Action=ChangeMessageVisibility &VisibilityTimeout=60
-- &ReceiptHandle=MbZj6wDWli%2BJvwwJaBV%2B3dcjk2YW2vA3%2BSTFFljT
-- M8tJJg6HRG6PYSasuWXPJB%2BCwLj1FjgXUv1uSj1gUPAWV66FU/WeR4mq2OKpEGY
-- WbnLmpRCJVAyeMjeU5ZBdtcQ%2BQEauMZc8ZRv37sIW2iJKq3M9MFx1YvV11A2x/K SbkJ0=
-- &Version=2009-02-01 &SignatureMethod=HmacSHA256
-- &Expires=2009-04-18T22%3A52%3A43PST &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &SignatureVersion=2 &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- 6a7a282a-d013-4a59-aba9-335b0fa48bed.
module Network.AWS.SQS.ChangeMessageVisibility
    (
    -- * Request
      ChangeMessageVisibility
    -- ** Request constructor
    , mkChangeMessageVisibility
    -- ** Request lenses
    , cmvQueueUrl
    , cmvReceiptHandle
    , cmvVisibilityTimeout

    -- * Response
    , ChangeMessageVisibilityResponse
    -- ** Response constructor
    , mkChangeMessageVisibilityResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SQS.Types
import Network.AWS.Prelude

data ChangeMessageVisibility = ChangeMessageVisibility
    { _cmvQueueUrl :: !Text
    , _cmvReceiptHandle :: !Text
    , _cmvVisibilityTimeout :: !Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ChangeMessageVisibility' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @QueueUrl ::@ @Text@
--
-- * @ReceiptHandle ::@ @Text@
--
-- * @VisibilityTimeout ::@ @Integer@
--
mkChangeMessageVisibility :: Text -- ^ 'cmvQueueUrl'
                          -> Text -- ^ 'cmvReceiptHandle'
                          -> Integer -- ^ 'cmvVisibilityTimeout'
                          -> ChangeMessageVisibility
mkChangeMessageVisibility p1 p2 p3 = ChangeMessageVisibility
    { _cmvQueueUrl = p1
    , _cmvReceiptHandle = p2
    , _cmvVisibilityTimeout = p3
    }

-- | The URL of the Amazon SQS queue to take action on.
cmvQueueUrl :: Lens' ChangeMessageVisibility Text
cmvQueueUrl = lens _cmvQueueUrl (\s a -> s { _cmvQueueUrl = a })

-- | The receipt handle associated with the message whose visibility timeout
-- should be changed. This parameter is returned by the ReceiveMessage action.
cmvReceiptHandle :: Lens' ChangeMessageVisibility Text
cmvReceiptHandle =
    lens _cmvReceiptHandle (\s a -> s { _cmvReceiptHandle = a })

-- | The new value (in seconds - from 0 to 43200 - maximum 12 hours) for the
-- message's visibility timeout.
cmvVisibilityTimeout :: Lens' ChangeMessageVisibility Integer
cmvVisibilityTimeout =
    lens _cmvVisibilityTimeout (\s a -> s { _cmvVisibilityTimeout = a })

instance ToQuery ChangeMessageVisibility where
    toQuery = genericQuery def

data ChangeMessageVisibilityResponse = ChangeMessageVisibilityResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ChangeMessageVisibilityResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkChangeMessageVisibilityResponse :: ChangeMessageVisibilityResponse
mkChangeMessageVisibilityResponse = ChangeMessageVisibilityResponse

instance AWSRequest ChangeMessageVisibility where
    type Sv ChangeMessageVisibility = SQS
    type Rs ChangeMessageVisibility = ChangeMessageVisibilityResponse

    request = post "ChangeMessageVisibility"
    response _ = nullaryResponse ChangeMessageVisibilityResponse