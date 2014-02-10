{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.CreateMultipartUpload
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Initiates a multipart upload and returns an upload ID.Note: After you
-- initiate multipart upload and upload one or more parts, you must either
-- complete or abort multipart upload in order to stop getting charged for
-- storage of the uploaded parts. Only after you either complete or abort
-- multipart upload, Amazon S3 frees up the parts storage and stops charging
-- you for the parts storage.
module Network.AWS.S3.CreateMultipartUpload where

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createMultipartUpload :: Text
                      -> Text
                      -> CreateMultipartUpload
createMultipartUpload p1 p2 = CreateMultipartUpload
    { cmuBucket = p1
    , cmuKey = p2
    , cmuACL = Nothing
    , cmuCacheControl = Nothing
    , cmuContentDisposition = Nothing
    , cmuContentEncoding = Nothing
    , cmuContentLanguage = Nothing
    , cmuContentType = Nothing
    , cmuExpires = Nothing
    , cmuGrantFullControl = Nothing
    , cmuGrantRead = Nothing
    , cmuGrantReadACP = Nothing
    , cmuGrantWriteACP = Nothing
    , cmuMetadata = mempty
    , cmuServerSideEncryption = Nothing
    , cmuStorageClass = Nothing
    , cmuWebsiteRedirectLocation = Nothing
    }

type InitiateMultipartUpload = CreateMultipartUpload
type InitiateMultipartUploadResponse = CreateMultipartUploadResponse

data CreateMultipartUpload = CreateMultipartUpload
    { cmuACL :: Maybe ACL
      -- ^ The canned ACL to apply to the object.
    , cmuBucket :: !Text
    , cmuCacheControl :: Maybe Text
      -- ^ Specifies caching behavior along the request/reply chain.
    , cmuContentDisposition :: Maybe Text
      -- ^ Specifies presentational information for the object.
    , cmuContentEncoding :: Maybe Text
      -- ^ Specifies what content encodings have been applied to the object and thus
      -- what decoding mechanisms must be applied to obtain the media-type
      -- referenced by the Content-Type header field.
    , cmuContentLanguage :: Maybe Text
      -- ^ The language the content is in.
    , cmuContentType :: Maybe Text
      -- ^ A standard MIME type describing the format of the object data.
    , cmuExpires :: Maybe UTCTime
      -- ^ The date and time at which the object is no longer cacheable.
    , cmuGrantFullControl :: Maybe Text
      -- ^ Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
    , cmuGrantRead :: Maybe Text
      -- ^ Allows grantee to read the object data and its metadata.
    , cmuGrantReadACP :: Maybe Text
      -- ^ Allows grantee to read the object ACL.
    , cmuGrantWriteACP :: Maybe Text
      -- ^ Allows grantee to write the ACL for the applicable object.
    , cmuKey :: !Text
    , cmuMetadata :: HashMap Text Text
      -- ^ A map of metadata to store with the object in S3.
    , cmuServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this object in S3.
    , cmuStorageClass :: Maybe StorageClass
      -- ^ The type of storage to use for the object. Defaults to 'STANDARD'.
    , cmuWebsiteRedirectLocation :: Maybe Text
      -- ^ If the bucket is configured as a website, redirects requests for this
      -- object to another object in the same bucket or to an external URL. Amazon
      -- S3 stores the value of this header in the object metadata.
    } deriving (Generic)

instance ToPath CreateMultipartUpload where
    toPath CreateMultipartUpload{..} = Text.concat
        [ "/"
        , toText cmuBucket
        , "/"
        , toText cmuKey
        ]

instance ToQuery CreateMultipartUpload where
    toQuery CreateMultipartUpload{..} = queryFromList
        [ "uploads"
        ]

instance ToHeaders CreateMultipartUpload where
    toHeaders CreateMultipartUpload{..} =
        [ "x-amz-acl" =: cmuACL
        , "Cache-Control" =: cmuCacheControl
        , "Content-Disposition" =: cmuContentDisposition
        , "Content-Encoding" =: cmuContentEncoding
        , "Content-Language" =: cmuContentLanguage
        , "Content-Type" =: cmuContentType
        , "Expires" =: cmuExpires
        , "x-amz-grant-full-control" =: cmuGrantFullControl
        , "x-amz-grant-read" =: cmuGrantRead
        , "x-amz-grant-read-acp" =: cmuGrantReadACP
        , "x-amz-grant-write-acp" =: cmuGrantWriteACP
        , "x-amz-server-side-encryption" =: cmuServerSideEncryption
        , "x-amz-storage-class" =: cmuStorageClass
        , "x-amz-website-redirect-location" =: cmuWebsiteRedirectLocation
        ]

instance AWSRequest CreateMultipartUpload where
    type Er CreateMultipartUpload = S3Error
    type Rs CreateMultipartUpload = CreateMultipartUploadResponse
    request rq = s3 POST (service $ cmuBucket rq) rq
    response = receiveXML $ \hs doc -> CreateMultipartUploadResponse
        <$> xml "Bucket" doc
        <*> xml "Key" doc
        <*> hdr "x-amz-server-side-encryption" hs
        <*> xml "UploadId" doc

data CreateMultipartUploadResponse = CreateMultipartUploadResponse
    { cmurBucket :: Maybe Text
      -- ^ Name of the bucket to which the multipart upload was initiated.
    , cmurKey :: Maybe Text
      -- ^ Object key for which the multipart upload was initiated.
    , cmurServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this object in S3.
    , cmurUploadId :: Maybe Text
      -- ^ ID for the initiated multipart upload.
    } deriving (Eq, Show)
