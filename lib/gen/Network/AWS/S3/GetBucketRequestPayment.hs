{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketRequestPayment
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the request payment configuration of a bucket.
module Network.AWS.S3.GetBucketRequestPayment where

import qualified Data.Text        as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
getBucketRequestPayment :: Text
                        -> GetBucketRequestPayment
getBucketRequestPayment p1 = GetBucketRequestPayment
    { gbrpBucket = p1
    }

data GetBucketRequestPayment = GetBucketRequestPayment
    { gbrpBucket :: !Text
    } deriving (Generic)

instance ToHeaders GetBucketRequestPayment

instance ToPath GetBucketRequestPayment where
    toPath GetBucketRequestPayment{..} = Text.concat
        [ "/"
        , toText gbrpBucket
        ]

instance ToQuery GetBucketRequestPayment where
    toQuery GetBucketRequestPayment{..} = queryFromList
        [ "requestPayment"
        ]

instance AWSRequest GetBucketRequestPayment where
    type Er GetBucketRequestPayment = S3Error
    type Rs GetBucketRequestPayment = GetBucketRequestPaymentResponse
    request  = getS3 service
    response = undefined

data GetBucketRequestPaymentResponse = GetBucketRequestPaymentResponse
    { gbrprPayer :: Maybe Payer
      -- ^ Specifies who pays for the download and request fees.
    } deriving (Eq, Show, Generic)

instance FromXML GetBucketRequestPaymentResponse where
    fromXMLOptions = xmlOptions
