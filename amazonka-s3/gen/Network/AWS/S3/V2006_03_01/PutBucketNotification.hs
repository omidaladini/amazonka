{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.PutBucketNotification
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables notifications of specified events for a bucket.
module Network.AWS.S3.V2006_03_01.PutBucketNotification where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'PutBucketNotification' request.
putBucketNotification :: NotificationConfiguration -- ^ '_pbnrNotificationConfiguration'
                      -> BucketName -- ^ '_pbnrBucket'
                      -> PutBucketNotification
putBucketNotification p1 p2 = PutBucketNotification
    { _pbnrNotificationConfiguration = p1
    , _pbnrBucket = p2
    , _pbnrContentMD5 = Nothing
    }

data PutBucketNotification = PutBucketNotification
    { _pbnrNotificationConfiguration :: NotificationConfiguration
    , _pbnrBucket :: BucketName
    , _pbnrContentMD5 :: Maybe Text
    } deriving (Show, Generic)

makeLenses ''PutBucketNotification

instance ToPath PutBucketNotification where
    toPath PutBucketNotification{..} = mconcat
        [ "/"
        , toBS _pbnrBucket
        ]

instance ToQuery PutBucketNotification where
    toQuery PutBucketNotification{..} = mconcat
        [ "notification"
        ]

instance ToHeaders PutBucketNotification where
    toHeaders PutBucketNotification{..} = concat
        [ "Content-MD5" =: _pbnrContentMD5
        ]

instance ToBody PutBucketNotification where
    toBody = toBody . encodeXML . _pbnrNotificationConfiguration

data PutBucketNotificationResponse = PutBucketNotificationResponse
    deriving (Eq, Show, Generic)

makeLenses ''PutBucketNotificationResponse

instance AWSRequest PutBucketNotification where
    type Sv PutBucketNotification = S3
    type Rs PutBucketNotification = PutBucketNotificationResponse

    request = put
    response _ = nullaryResponse PutBucketNotificationResponse