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

instance ToHeaders GetObject where
    toHeaders GetObject{..} =
        [ "If-Match" =: goIfMatch
        , "If-Modified-Since" =: goIfModifiedSince
        , "If-None-Match" =: goIfNoneMatch
        , "If-Unmodified-Since" =: goIfUnmodifiedSince
        , "Range" =: goRange
        ]

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

instance AWSRequest GetObject where
    type Er GetObject = S3Error
    type Rs GetObject = GetObjectResponse
    request  = getS3 service
    response = undefined

data GetObjectResponse = GetObjectResponse
    { gorsAcceptRanges :: Maybe Text
    , gorsBody :: ResumableSource AWS ByteString
      -- ^ Object data.
    , gorsCacheControl :: Maybe Text
      -- ^ Specifies caching behavior along the request/reply chain.
    , gorsContentDisposition :: Maybe Text
      -- ^ Specifies presentational information for the object.
    , gorsContentEncoding :: Maybe Text
      -- ^ Specifies what content encodings have been applied to the object and thus
      -- what decoding mechanisms must be applied to obtain the media-type
      -- referenced by the Content-Type header field.
    , gorsContentLanguage :: Maybe Text
      -- ^ The language the content is in.
    , gorsContentLength :: Maybe Int
      -- ^ Size of the body in bytes.
    , gorsContentType :: Maybe Text
      -- ^ A standard MIME type describing the format of the object data.
    , gorsDeleteMarker :: Maybe Bool
      -- ^ Specifies whether the object retrieved was (true) or was not (false) a
      -- Delete Marker. If false, this response header does not appear in the
      -- response.
    , gorsETag :: Maybe Text
      -- ^ An ETag is an opaque identifier assigned by a web server to a specific
      -- version of a resource found at a URL.
    , gorsExpiration :: Maybe Text
      -- ^ If the object expiration is configured (see PUT Bucket lifecycle), the
      -- response includes this header. It includes the expiry-date and rule-id key
      -- value pairs providing object expiration information. The value of the
      -- rule-id is URL encoded.
    , gorsExpires :: Maybe UTCTime
      -- ^ The date and time at which the object is no longer cacheable.
    , gorsLastModified :: Maybe UTCTime
      -- ^ Last modified date of the object.
    , gorsMetadata :: HashMap Text Text
      -- ^ A map of metadata to store with the object in S3.
    , gorsMissingMeta :: Maybe Int
      -- ^ This is set to the number of metadata entries not returned in x-amz-meta
      -- headers. This can happen if you create metadata using an API like SOAP that
      -- supports more flexible metadata than the REST API. For example, using SOAP,
      -- you can create metadata whose values are not legal HTTP headers.
    , gorsRestore :: Maybe Text
      -- ^ Provides information about object restoration operation and expiration time
      -- of the restored object copy.
    , gorsServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this object in S3.
    , gorsVersionId :: Maybe Text
      -- ^ Version of the object.
    , gorsWebsiteRedirectLocation :: Maybe Text
      -- ^ If the bucket is configured as a website, redirects requests for this
      -- object to another object in the same bucket or to an external URL. Amazon
      -- S3 stores the value of this header in the object metadata.
    }
