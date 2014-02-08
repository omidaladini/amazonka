{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutBucketRequestPayment
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the request payment configuration for a bucket. By default, the bucket
-- owner pays for downloads from the bucket. This configuration parameter
-- enables the bucket owner (only) to specify that the person requesting the
-- download will be charged for the download.
module Network.AWS.S3.PutBucketRequestPayment where

import qualified Data.Text        as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
putBucketRequestPayment :: Text
                        -> RequestPaymentConfiguration
                        -> PutBucketRequestPayment
putBucketRequestPayment p1 p2 = PutBucketRequestPayment
    { pbrpBucket = p1
    , pbrpRequestPaymentConfiguration = p2
    , pbrpContentMD5 = Nothing
    }

data PutBucketRequestPayment = PutBucketRequestPayment
    { pbrpBucket :: !Text
    , pbrpContentMD5 :: Maybe Text
    , pbrpRequestPaymentConfiguration :: RequestPaymentConfiguration
    } deriving (Generic)

instance ToHeaders PutBucketRequestPayment where
    toHeaders PutBucketRequestPayment{..} =
        [ "Content-MD5" =: pbrpContentMD5
        ]

instance ToPath PutBucketRequestPayment where
    toPath PutBucketRequestPayment{..} = Text.concat
        [ "/"
        , toText pbrpBucket
        ]

instance ToQuery PutBucketRequestPayment where
    toQuery PutBucketRequestPayment{..} = queryFromList
        [ "requestPayment"
        ]

instance AWSRequest PutBucketRequestPayment where
    type Er PutBucketRequestPayment = S3Error
    type Rs PutBucketRequestPayment = PutBucketRequestPaymentResponse
    request  = putS3 service
    response = undefined

data PutBucketRequestPaymentResponse = PutBucketRequestPaymentResponse
    deriving (Eq, Show, Generic)

instance FromXML PutBucketRequestPaymentResponse where
    fromXMLOptions = xmlOptions
