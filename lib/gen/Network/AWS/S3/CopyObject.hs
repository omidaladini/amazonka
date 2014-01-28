{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.CopyObject
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a copy of an object that is already stored in Amazon S3.
module Network.AWS.S3.CopyObject where

import           Control.Monad
import           Data.ByteString                  (ByteString)
import           Data.Conduit
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as Map
import           Data.List.NonEmpty               (NonEmpty(..))
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Time                        (UTCTime)
import           GHC.Generics                     (Generic)
import           Network.AWS.Internal             hiding (Endpoint, Region, AvailabilityZone)
import           Network.HTTP.Conduit
import           Network.HTTP.QueryString.Generic (Query(List))

import Network.AWS.S3.Service
import Network.AWS.S3.Types

type PutObjectCopy = CopyObject
type PutObjectCopyResponse = CopyObjectResponse

data CopyObject = CopyObject
    { coACL :: Maybe ACL
      -- ^ The canned ACL to apply to the object.
    , coBucket :: !Text
    , coCacheControl :: Maybe Text
      -- ^ Specifies caching behavior along the request/reply chain.
    , coContentDisposition :: Maybe Text
      -- ^ Specifies presentational information for the object.
    , coContentEncoding :: Maybe Text
      -- ^ Specifies what content encodings have been applied to the object and thus
      -- what decoding mechanisms must be applied to obtain the media-type
      -- referenced by the Content-Type header field.
    , coContentLanguage :: Maybe Text
      -- ^ The language the content is in.
    , coContentType :: Maybe Text
      -- ^ A standard MIME type describing the format of the object data.
    , coCopySource :: !Text
      -- ^ The name of the source bucket and key name of the source object, separated
      -- by a slash (/). Must be URL-encoded.
    , coCopySourceIfMatch :: Maybe UTCTime
      -- ^ Copies the object if its entity tag (ETag) matches the specified tag.
    , coCopySourceIfModifiedSince :: Maybe UTCTime
      -- ^ Copies the object if it has been modified since the specified time.
    , coCopySourceIfNoneMatch :: Maybe UTCTime
      -- ^ Copies the object if its entity tag (ETag) is different than the specified
      -- ETag.
    , coCopySourceIfUnmodifiedSince :: Maybe UTCTime
      -- ^ Copies the object if it hasn't been modified since the specified time.
    , coExpires :: Maybe UTCTime
      -- ^ The date and time at which the object is no longer cacheable.
    , coGrantFullControl :: Maybe Text
      -- ^ Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
    , coGrantRead :: Maybe Text
      -- ^ Allows grantee to read the object data and its metadata.
    , coGrantReadACP :: Maybe Text
      -- ^ Allows grantee to read the object ACL.
    , coGrantWriteACP :: Maybe Text
      -- ^ Allows grantee to write the ACL for the applicable object.
    , coKey :: !Text
    , coMetadata :: HashMap Text Text
      -- ^ A map of metadata to store with the object in S3.
    , coMetadataDirective :: Maybe MetadataDirective
      -- ^ Specifies whether the metadata is copied from the source object or replaced
      -- with metadata provided in the request.
    , coServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this object in S3.
    , coStorageClass :: Maybe StorageClass
      -- ^ The type of storage to use for the object. Defaults to 'STANDARD'.
    , coWebsiteRedirectLocation :: Maybe Text
      -- ^ If the bucket is configured as a website, redirects requests for this
      -- object to another object in the same bucket or to an external URL. Amazon
      -- S3 stores the value of this header in the object metadata.
    } deriving (Generic)

instance ToHeaders CopyObject where
    toHeaders CopyObject{..} =
        [ "x-amz-acl" =: coACL
        , "Cache-Control" =: coCacheControl
        , "Content-Disposition" =: coContentDisposition
        , "Content-Encoding" =: coContentEncoding
        , "Content-Language" =: coContentLanguage
        , "Content-Type" =: coContentType
        , "x-amz-copy-source" =: coCopySource
        , "x-amz-copy-source-if-match" =: coCopySourceIfMatch
        , "x-amz-copy-source-if-modified-since" =: coCopySourceIfModifiedSince
        , "x-amz-copy-source-if-none-match" =: coCopySourceIfNoneMatch
        , "x-amz-copy-source-if-unmodified-since" =: coCopySourceIfUnmodifiedSince
        , "Expires" =: coExpires
        , "x-amz-grant-full-control" =: coGrantFullControl
        , "x-amz-grant-read" =: coGrantRead
        , "x-amz-grant-read-acp" =: coGrantReadACP
        , "x-amz-grant-write-acp" =: coGrantWriteACP
        , "x-amz-metadata-directive" =: coMetadataDirective
        , "x-amz-server-side-encryption" =: coServerSideEncryption
        , "x-amz-storage-class" =: coStorageClass
        , "x-amz-website-redirect-location" =: coWebsiteRedirectLocation
        ]

instance ToPath CopyObject where
    toPath CopyObject{..} = Text.concat
        [ "/"
        , toText coBucket
        , "/"
        , toText coKey
        ]

instance ToQuery CopyObject where
    toQuery = const mempty

instance AWSRequest CopyObject where
    type Er CopyObject = S3Error
    type Rs CopyObject = CopyObjectResponse
    request  = putS3 service
    response = undefined

data CopyObjectResponse = CopyObjectResponse
    { corsCopyObjectResult :: Maybe CopyObjectResult
    , corsCopySourceVersionId :: Maybe Text
    , corsExpiration :: Maybe Text
      -- ^ If the object expiration is configured, the response includes this header.
    , corsServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this object in S3.
    } deriving (Eq, Show, Generic)

instance FromXML CopyObjectResponse where
    fromXMLOptions = xmlOptions

copyObject :: Text -- ^ Bucket
           -> Text -- ^ Key
           -> Text -- ^ CopySource
           -> AWS (Either S3Error CopyObjectResponse)
copyObject p1 p2 p3 = undefined $ CopyObject
    { coBucket = p1
    , coKey = p2
    , coCopySource = p3
    , coACL = Nothing
    , coCacheControl = Nothing
    , coContentDisposition = Nothing
    , coContentEncoding = Nothing
    , coContentLanguage = Nothing
    , coContentType = Nothing
    , coCopySourceIfMatch = Nothing
    , coCopySourceIfModifiedSince = Nothing
    , coCopySourceIfNoneMatch = Nothing
    , coCopySourceIfUnmodifiedSince = Nothing
    , coExpires = Nothing
    , coGrantFullControl = Nothing
    , coGrantRead = Nothing
    , coGrantReadACP = Nothing
    , coGrantWriteACP = Nothing
    , coMetadata = mempty
    , coMetadataDirective = Nothing
    , coServerSideEncryption = Nothing
    , coStorageClass = Nothing
    , coWebsiteRedirectLocation = Nothing
    }
