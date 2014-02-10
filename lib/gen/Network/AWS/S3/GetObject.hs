{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetObject
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves objects from Amazon S3.
module Network.AWS.S3.GetObject where

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
getObject :: Text
          -> Text
          -> GetObject
getObject p1 p2 = GetObject
    { goBucket = p1
    , goKey = p2
    , goIfMatch = Nothing
    , goIfModifiedSince = Nothing
    , goIfNoneMatch = Nothing
    , goIfUnmodifiedSince = Nothing
    , goRange = Nothing
    , goResponseCacheControl = Nothing
    , goResponseContentDisposition = Nothing
    , goResponseContentEncoding = Nothing
    , goResponseContentLanguage = Nothing
    , goResponseContentType = Nothing
    , goResponseExpires = Nothing
    , goVersionId = Nothing
    }

data GetObject = GetObject
    { goBucket :: !Text
    , goIfMatch :: Maybe Text
      -- ^ Return the object only if its entity tag (ETag) is the same as the one
      -- specified, otherwise return a 412 (precondition failed).
    , goIfModifiedSince :: Maybe UTCTime
      -- ^ Return the object only if it has been modified since the specified time,
      -- otherwise return a 304 (not modified).
    , goIfNoneMatch :: Maybe Text
      -- ^ Return the object only if its entity tag (ETag) is different from the one
      -- specified, otherwise return a 304 (not modified).
    , goIfUnmodifiedSince :: Maybe UTCTime
      -- ^ Return the object only if it has not been modified since the specified
      -- time, otherwise return a 412 (precondition failed).
    , goKey :: !Text
    , goRange :: Maybe Text
      -- ^ Downloads the specified range bytes of an object. For more information
      -- about the HTTP Range header, go to
      -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35.
    , goResponseCacheControl :: Maybe Text
      -- ^ Sets the Cache-Control header of the response.
    , goResponseContentDisposition :: Maybe Text
      -- ^ Sets the Content-Disposition header of the response.
    , goResponseContentEncoding :: Maybe Text
      -- ^ Sets the Content-Encoding header of the response.
    , goResponseContentLanguage :: Maybe Text
      -- ^ Sets the Content-Language header of the response.
    , goResponseContentType :: Maybe Text
      -- ^ Sets the Content-Type header of the response.
    , goResponseExpires :: Maybe UTCTime
      -- ^ Sets the Expires header of the response.
    , goVersionId :: Maybe Text
      -- ^ VersionId used to reference a specific version of the object.
    } deriving (Generic)

instance ToPath GetObject where
    toPath GetObject{..} = Text.concat
        [ "/"
        , toText goBucket
        , "/"
        , toText goKey
        ]

instance ToQuery GetObject where
    toQuery GetObject{..} = queryFromList
        [ "response-cache-control" =? goResponseCacheControl
        , "response-content-disposition" =? goResponseContentDisposition
        , "response-content-encoding" =? goResponseContentEncoding
        , "response-content-language" =? goResponseContentLanguage
        , "response-content-type" =? goResponseContentType
        , "response-expires" =? goResponseExpires
        , "versionId" =? goVersionId
        ]

instance ToHeaders GetObject where
    toHeaders GetObject{..} =
        [ "If-Match" =: goIfMatch
        , "If-Modified-Since" =: goIfModifiedSince
        , "If-None-Match" =: goIfNoneMatch
        , "If-Unmodified-Since" =: goIfUnmodifiedSince
        , "Range" =: goRange
        ]

instance AWSRequest GetObject where
    type Er GetObject = S3Error
    type Rs GetObject = GetObjectResponse
    request rq = s3 GET (service $ goBucket rq) rq
    response = receiveBody $ \hs bdy -> GetObjectResponse
        <$> hdr "accept-ranges" hs
        <*> pure bdy
        <*> hdr "Cache-Control" hs
        <*> hdr "Content-Disposition" hs
        <*> hdr "Content-Encoding" hs
        <*> hdr "Content-Language" hs
        <*> hdr "Content-Length" hs
        <*> hdr "Content-Type" hs
        <*> hdr "x-amz-delete-marker" hs
        <*> hdr "ETag" hs
        <*> hdr "x-amz-expiration" hs
        <*> hdr "Expires" hs
        <*> hdr "Last-Modified" hs
        <*> hdrs "x-amz-meta-" hs
        <*> hdr "x-amz-missing-meta" hs
        <*> hdr "x-amz-restore" hs
        <*> hdr "x-amz-server-side-encryption" hs
        <*> hdr "x-amz-version-id" hs
        <*> hdr "x-amz-website-redirect-location" hs

data GetObjectResponse = GetObjectResponse
    { gorAcceptRanges :: Maybe Text
    , gorBody :: ResumableSource AWS ByteString
      -- ^ Object data.
    , gorCacheControl :: Maybe Text
      -- ^ Specifies caching behavior along the request/reply chain.
    , gorContentDisposition :: Maybe Text
      -- ^ Specifies presentational information for the object.
    , gorContentEncoding :: Maybe Text
      -- ^ Specifies what content encodings have been applied to the object and thus
      -- what decoding mechanisms must be applied to obtain the media-type
      -- referenced by the Content-Type header field.
    , gorContentLanguage :: Maybe Text
      -- ^ The language the content is in.
    , gorContentLength :: Maybe Int
      -- ^ Size of the body in bytes.
    , gorContentType :: Maybe Text
      -- ^ A standard MIME type describing the format of the object data.
    , gorDeleteMarker :: Maybe Bool
      -- ^ Specifies whether the object retrieved was (true) or was not (false) a
      -- Delete Marker. If false, this response header does not appear in the
      -- response.
    , gorETag :: Maybe Text
      -- ^ An ETag is an opaque identifier assigned by a web server to a specific
      -- version of a resource found at a URL.
    , gorExpiration :: Maybe Text
      -- ^ If the object expiration is configured (see PUT Bucket lifecycle), the
      -- response includes this header. It includes the expiry-date and rule-id key
      -- value pairs providing object expiration information. The value of the
      -- rule-id is URL encoded.
    , gorExpires :: Maybe UTCTime
      -- ^ The date and time at which the object is no longer cacheable.
    , gorLastModified :: Maybe UTCTime
      -- ^ Last modified date of the object.
    , gorMetadata :: HashMap Text Text
      -- ^ A map of metadata to store with the object in S3.
    , gorMissingMeta :: Maybe Int
      -- ^ This is set to the number of metadata entries not returned in x-amz-meta
      -- headers. This can happen if you create metadata using an API like SOAP that
      -- supports more flexible metadata than the REST API. For example, using SOAP,
      -- you can create metadata whose values are not legal HTTP headers.
    , gorRestore :: Maybe Text
      -- ^ Provides information about object restoration operation and expiration time
      -- of the restored object copy.
    , gorServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this object in S3.
    , gorVersionId :: Maybe Text
      -- ^ Version of the object.
    , gorWebsiteRedirectLocation :: Maybe Text
      -- ^ If the bucket is configured as a website, redirects requests for this
      -- object to another object in the same bucket or to an external URL. Amazon
      -- S3 stores the value of this header in the object metadata.
    } deriving (Show)
