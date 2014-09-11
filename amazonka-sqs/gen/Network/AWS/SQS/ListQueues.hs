{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.ListQueues
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of your queues. The maximum number of queues that can be
-- returned is 1000. If you specify a value for the optional QueueNamePrefix
-- parameter, only queues with a name beginning with the specified value are
-- returned. The following example Query request returns the queues whose
-- names begin with the letter "T". http://sqs.us-east-1.amazonaws.com/
-- ?Action=ListQueues &QueueNamePrefix=t &Version=2009-02-01
-- &SignatureMethod=HmacSHA256 &Expires=2009-04-18T22%3A52%3A43PST
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &SignatureVersion=2
-- &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- http://sqs.us-east-1.amazonaws.com/123456789012/testQueue
-- 725275ae-0b9b-4762-b238-436d7c65a1ac.
module Network.AWS.SQS.ListQueues
    (
    -- * Request
      ListQueues
    -- ** Request constructor
    , mkListQueues
    -- ** Request lenses
    , lqQueueNamePrefix

    -- * Response
    , ListQueuesResponse
    -- ** Response constructor
    , mkListQueuesResponse
    -- ** Response lenses
    , lqrQueueUrls
    ) where

import Network.AWS.Request.Query
import Network.AWS.SQS.Types
import Network.AWS.Prelude

newtype ListQueues = ListQueues
    { _lqQueueNamePrefix :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListQueues' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @QueueNamePrefix ::@ @Maybe Text@
--
mkListQueues :: ListQueues
mkListQueues = ListQueues
    { _lqQueueNamePrefix = Nothing
    }

-- | A string to use for filtering the list results. Only those queues whose
-- name begins with the specified string are returned.
lqQueueNamePrefix :: Lens' ListQueues (Maybe Text)
lqQueueNamePrefix =
    lens _lqQueueNamePrefix (\s a -> s { _lqQueueNamePrefix = a })

instance ToQuery ListQueues where
    toQuery = genericQuery def

-- | A list of your queues.
newtype ListQueuesResponse = ListQueuesResponse
    { _lqrQueueUrls :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListQueuesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @QueueUrls ::@ @[Text]@
--
mkListQueuesResponse :: ListQueuesResponse
mkListQueuesResponse = ListQueuesResponse
    { _lqrQueueUrls = mempty
    }

-- | A list of queue URLs, up to 1000 entries.
lqrQueueUrls :: Lens' ListQueuesResponse [Text]
lqrQueueUrls = lens _lqrQueueUrls (\s a -> s { _lqrQueueUrls = a })

instance FromXML ListQueuesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListQueues where
    type Sv ListQueues = SQS
    type Rs ListQueues = ListQueuesResponse

    request = post "ListQueues"
    response _ = xmlResponse