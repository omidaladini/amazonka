{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.UploadPartCopy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Uploads a part by copying data from an existing object as data source.
module Network.AWS.S3.UploadPartCopy where

import qualified Data.Text        as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
uploadPartCopy :: Text
               -> Text
               -> Text
               -- ^ The name of the source bucket and key name of the source object, separated
               -- by a slash (/). Must be URL-encoded.
               -> Int
               -- ^ Part number of part being copied.
               -> Text
               -- ^ Upload ID identifying the multipart upload whose part is being copied.
               -> UploadPartCopy
uploadPartCopy p1 p2 p3 p4 p5 = UploadPartCopy
    { upcBucket = p1
    , upcKey = p2
    , upcCopySource = p3
    , upcPartNumber = p4
    , upcUploadId = p5
    , upcCopySourceIfMatch = Nothing
    , upcCopySourceIfModifiedSince = Nothing
    , upcCopySourceIfNoneMatch = Nothing
    , upcCopySourceIfUnmodifiedSince = Nothing
    , upcCopySourceRange = Nothing
    }

data UploadPartCopy = UploadPartCopy
    { upcBucket :: !Text
    , upcCopySource :: !Text
      -- ^ The name of the source bucket and key name of the source object, separated
      -- by a slash (/). Must be URL-encoded.
    , upcCopySourceIfMatch :: Maybe UTCTime
      -- ^ Copies the object if its entity tag (ETag) matches the specified tag.
    , upcCopySourceIfModifiedSince :: Maybe UTCTime
      -- ^ Copies the object if it has been modified since the specified time.
    , upcCopySourceIfNoneMatch :: Maybe UTCTime
      -- ^ Copies the object if its entity tag (ETag) is different than the specified
      -- ETag.
    , upcCopySourceIfUnmodifiedSince :: Maybe UTCTime
      -- ^ Copies the object if it hasn't been modified since the specified time.
    , upcCopySourceRange :: Maybe Text
      -- ^ The range of bytes to copy from the source object. The range value must use
      -- the form bytes=first-last, where the first and last are the zero-based byte
      -- offsets to copy. For example, bytes=0-9 indicates that you want to copy the
      -- first ten bytes of the source. You can copy a range only if the source
      -- object is greater than 5 GB.
    , upcKey :: !Text
    , upcPartNumber :: !Int
      -- ^ Part number of part being copied.
    , upcUploadId :: !Text
      -- ^ Upload ID identifying the multipart upload whose part is being copied.
    } deriving (Generic)

instance ToHeaders UploadPartCopy where
    toHeaders UploadPartCopy{..} =
        [ "x-amz-copy-source" =: upcCopySource
        , "x-amz-copy-source-if-match" =: upcCopySourceIfMatch
        , "x-amz-copy-source-if-modified-since" =: upcCopySourceIfModifiedSince
        , "x-amz-copy-source-if-none-match" =: upcCopySourceIfNoneMatch
        , "x-amz-copy-source-if-unmodified-since" =: upcCopySourceIfUnmodifiedSince
        , "x-amz-copy-source-range" =: upcCopySourceRange
        ]

instance ToPath UploadPartCopy where
    toPath UploadPartCopy{..} = Text.concat
        [ "/"
        , toText upcBucket
        , "/"
        , toText upcKey
        ]

instance ToQuery UploadPartCopy where
    toQuery UploadPartCopy{..} = queryFromList
        [ "partNumber" =? upcPartNumber
        , "uploadId" =? upcUploadId
        ]

instance AWSRequest UploadPartCopy where
    type Er UploadPartCopy = S3Error
    type Rs UploadPartCopy = UploadPartCopyResponse
    request  = putS3 service
    response = undefined

data UploadPartCopyResponse = UploadPartCopyResponse
    { upcrsCopyPartResult :: Maybe CopyPartResult
    , upcrsCopySourceVersionId :: Maybe Text
      -- ^ The version of the source object that was copied, if you have enabled
      -- versioning on the source bucket.
    , upcrsServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this object in S3.
    } deriving (Eq, Show, Generic)

instance FromXML UploadPartCopyResponse where
    fromXMLOptions = xmlOptions
