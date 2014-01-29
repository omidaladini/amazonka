{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.HeadObject
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The HEAD operation retrieves metadata from an object without returning the
-- object itself. This operation is useful if you're only interested in an
-- object's metadata. To use HEAD, you must have READ access to the object.
module Network.AWS.S3.HeadObject where

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

-- | Convenience method utilising default fields where applicable.
headObject :: Text -- ^ Bucket
           -> Text -- ^ Key
           -> AWS (Either S3Error HeadObjectResponse)
headObject p1 p2 = undefined $ HeadObject
    { hoBucket = p1
    , hoKey = p2
    , hoIfMatch = Nothing
    , hoIfModifiedSince = Nothing
    , hoIfNoneMatch = Nothing
    , hoIfUnmodifiedSince = Nothing
    , hoRange = Nothing
    , hoVersionId = Nothing
    }

data HeadObject = HeadObject
    { hoBucket :: !Text
    , hoIfMatch :: Maybe Text
      -- ^ Return the object only if its entity tag (ETag) is the same as the one
      -- specified, otherwise return a 412 (precondition failed).
    , hoIfModifiedSince :: Maybe UTCTime
      -- ^ Return the object only if it has been modified since the specified time,
      -- otherwise return a 304 (not modified).
    , hoIfNoneMatch :: Maybe Text
      -- ^ Return the object only if its entity tag (ETag) is different from the one
      -- specified, otherwise return a 304 (not modified).
    , hoIfUnmodifiedSince :: Maybe UTCTime
      -- ^ Return the object only if it has not been modified since the specified
      -- time, otherwise return a 412 (precondition failed).
    , hoKey :: !Text
    , hoRange :: Maybe Text
      -- ^ Downloads the specified range bytes of an object. For more information
      -- about the HTTP Range header, go to
      -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35.
    , hoVersionId :: Maybe Text
      -- ^ VersionId used to reference a specific version of the object.
    } deriving (Generic)

instance ToHeaders HeadObject where
    toHeaders HeadObject{..} =
        [ "If-Match" =: hoIfMatch
        , "If-Modified-Since" =: hoIfModifiedSince
        , "If-None-Match" =: hoIfNoneMatch
        , "If-Unmodified-Since" =: hoIfUnmodifiedSince
        , "Range" =: hoRange
        ]

instance ToPath HeadObject where
    toPath HeadObject{..} = Text.concat
        [ "/"
        , toText hoBucket
        , "/"
        , toText hoKey
        ]

instance ToQuery HeadObject where
    toQuery HeadObject{..} = List
        [ "versionId" =? hoVersionId
        ]

instance AWSRequest HeadObject where
    type Er HeadObject = S3Error
    type Rs HeadObject = HeadObjectResponse
    request  = headS3 service
    response = undefined

data HeadObjectResponse = HeadObjectResponse
    { horsAcceptRanges :: Maybe Text
    , horsCacheControl :: Maybe Text
      -- ^ Specifies caching behavior along the request/reply chain.
    , horsContentDisposition :: Maybe Text
      -- ^ Specifies presentational information for the object.
    , horsContentEncoding :: Maybe Text
      -- ^ Specifies what content encodings have been applied to the object and thus
      -- what decoding mechanisms must be applied to obtain the media-type
      -- referenced by the Content-Type header field.
    , horsContentLanguage :: Maybe Text
      -- ^ The language the content is in.
    , horsContentLength :: Maybe Int
      -- ^ Size of the body in bytes.
    , horsContentType :: Maybe Text
      -- ^ A standard MIME type describing the format of the object data.
    , horsDeleteMarker :: Maybe Bool
      -- ^ Specifies whether the object retrieved was (true) or was not (false) a
      -- Delete Marker. If false, this response header does not appear in the
      -- response.
    , horsETag :: Maybe Text
      -- ^ An ETag is an opaque identifier assigned by a web server to a specific
      -- version of a resource found at a URL.
    , horsExpiration :: Maybe Text
      -- ^ If the object expiration is configured (see PUT Bucket lifecycle), the
      -- response includes this header. It includes the expiry-date and rule-id key
      -- value pairs providing object expiration information. The value of the
      -- rule-id is URL encoded.
    , horsExpires :: Maybe UTCTime
      -- ^ The date and time at which the object is no longer cacheable.
    , horsLastModified :: Maybe UTCTime
      -- ^ Last modified date of the object.
    , horsMetadata :: HashMap Text Text
      -- ^ A map of metadata to store with the object in S3.
    , horsMissingMeta :: Maybe Int
      -- ^ This is set to the number of metadata entries not returned in x-amz-meta
      -- headers. This can happen if you create metadata using an API like SOAP that
      -- supports more flexible metadata than the REST API. For example, using SOAP,
      -- you can create metadata whose values are not legal HTTP headers.
    , horsRestore :: Maybe Text
      -- ^ Provides information about object restoration operation and expiration time
      -- of the restored object copy.
    , horsServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this object in S3.
    , horsVersionId :: Maybe Text
      -- ^ Version of the object.
    , horsWebsiteRedirectLocation :: Maybe Text
      -- ^ If the bucket is configured as a website, redirects requests for this
      -- object to another object in the same bucket or to an external URL. Amazon
      -- S3 stores the value of this header in the object metadata.
    } deriving (Eq, Show, Generic)

instance FromXML HeadObjectResponse where
    fromXMLOptions = xmlOptions
