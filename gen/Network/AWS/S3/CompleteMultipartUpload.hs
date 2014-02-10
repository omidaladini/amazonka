{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.CompleteMultipartUpload
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Completes a multipart upload by assembling previously uploaded parts.
module Network.AWS.S3.CompleteMultipartUpload where

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
completeMultipartUpload :: Text
                        -> Text
                        -> Text
                        -> CompleteMultipartUpload
completeMultipartUpload p1 p2 p3 = CompleteMultipartUpload
    { muBucket = p1
    , muKey = p2
    , muUploadId = p3
    , muCompleteMultipartUpload = Nothing
    }

data CompleteMultipartUpload = CompleteMultipartUpload
    { muBucket :: !Text
    , muKey :: !Text
    , muCompleteMultipartUpload :: Maybe MultipartUpload
    , muUploadId :: !Text
    } deriving (Generic)

instance ToPath CompleteMultipartUpload where
    toPath CompleteMultipartUpload{..} = Text.concat
        [ "/"
        , toText muBucket
        , "/"
        , toText muKey
        ]

instance ToQuery CompleteMultipartUpload where
    toQuery CompleteMultipartUpload{..} = queryFromList
        [ "uploadId" =? muUploadId
        ]

instance ToHeaders CompleteMultipartUpload

instance AWSRequest CompleteMultipartUpload where
    type Er CompleteMultipartUpload = S3Error
    type Rs CompleteMultipartUpload = CompleteMultipartUploadResponse
    request rq = s3XML POST (service $ muBucket rq) (muCompleteMultipartUpload rq) rq
    response = receiveXML $ \hs doc -> CompleteMultipartUploadResponse
        <$> xml "Bucket" doc
        <*> xml "ETag" doc
        <*> hdr "x-amz-expiration" hs
        <*> xml "Key" doc
        <*> xml "Location" doc
        <*> hdr "x-amz-server-side-encryption" hs
        <*> hdr "x-amz-version-id" hs

data CompleteMultipartUploadResponse = CompleteMultipartUploadResponse
    { murBucket :: Maybe Text
    , murETag :: Maybe Text
      -- ^ Entity tag of the object.
    , murExpiration :: Maybe UTCTime
      -- ^ If the object expiration is configured, this will contain the expiration
      -- date (expiry-date) and rule ID (rule-id). The value of rule-id is URL
      -- encoded.
    , murKey :: Maybe Text
    , murLocation :: Maybe Text
    , murServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this object in S3.
    , murVersionId :: Maybe Text
      -- ^ Version of the object.
    } deriving (Eq, Show)
