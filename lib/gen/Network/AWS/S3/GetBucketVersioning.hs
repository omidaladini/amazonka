{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketVersioning
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the versioning state of a bucket.
module Network.AWS.S3.GetBucketVersioning where

import qualified Data.Text        as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
getBucketVersioning :: Text
                    -> GetBucketVersioning
getBucketVersioning p1 = GetBucketVersioning
    { gbvBucket = p1
    }

data GetBucketVersioning = GetBucketVersioning
    { gbvBucket :: !Text
    } deriving (Generic)

instance ToHeaders GetBucketVersioning

instance ToPath GetBucketVersioning where
    toPath GetBucketVersioning{..} = Text.concat
        [ "/"
        , toText gbvBucket
        ]

instance ToQuery GetBucketVersioning where
    toQuery GetBucketVersioning{..} = queryFromList
        [ "versioning"
        ]

instance AWSRequest GetBucketVersioning where
    type Er GetBucketVersioning = S3Error
    type Rs GetBucketVersioning = GetBucketVersioningResponse
    request  = getS3 service
    response = undefined

data GetBucketVersioningResponse = GetBucketVersioningResponse
    { gbvrMfaDelete :: Maybe MfaDelete
      -- ^ Specifies whether MFA delete is enabled in the bucket versioning
      -- configuration. This element is only returned if the bucket has been
      -- configured with MFA delete. If the bucket has never been so configured,
      -- this element is not returned.
    , gbvrStatus :: Maybe Status
      -- ^ The versioning state of the bucket.
    } deriving (Eq, Show, Generic)

instance FromXML GetBucketVersioningResponse where
    fromXMLOptions = xmlOptions
