{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.ListMultipartUploads
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation lists in-progress multipart uploads.
module Network.AWS.S3.ListMultipartUploads where

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
listMultipartUploads :: Text
                     -> ListMultipartUploads
listMultipartUploads p1 = ListMultipartUploads
    { lmuBucket = p1
    , lmuDelimiter = Nothing
    , lmuEncodingType = Nothing
    , lmuKeyMarker = Nothing
    , lmuMaxUploads = Nothing
    , lmuPrefix = Nothing
    , lmuUploadIdMarker = Nothing
    }

data ListMultipartUploads = ListMultipartUploads
    { lmuBucket :: !Text
    , lmuDelimiter :: Maybe Text
      -- ^ Character you use to group keys.
    , lmuEncodingType :: Maybe EncodingType
      -- ^ Requests Amazon S3 to encode the object keys in the response and specifies
      -- the encoding method to use. An object key may contain any Unicode
      -- character; however, XML 1.0 parser cannot parse some characters, such as
      -- characters with an ASCII value from 0 to 10. For characters that are not
      -- supported in XML 1.0, you can add this parameter to request that Amazon S3
      -- encode the keys in the response.
    , lmuKeyMarker :: Maybe Text
      -- ^ Together with upload-id-marker, this parameter specifies the multipart
      -- upload after which listing should begin.
    , lmuMaxUploads :: Maybe Int
      -- ^ Sets the maximum number of multipart uploads, from 1 to 1,000, to return in
      -- the response body. 1,000 is the maximum number of uploads that can be
      -- returned in a response.
    , lmuPrefix :: Maybe Text
      -- ^ Lists in-progress uploads only for those keys that begin with the specified
      -- prefix.
    , lmuUploadIdMarker :: Maybe Text
      -- ^ Together with key-marker, specifies the multipart upload after which
      -- listing should begin. If key-marker is not specified, the upload-id-marker
      -- parameter is ignored.
    } deriving (Generic)

instance ToPath ListMultipartUploads where
    toPath ListMultipartUploads{..} = Text.concat
        [ "/"
        , toText lmuBucket
        ]

instance ToQuery ListMultipartUploads where
    toQuery ListMultipartUploads{..} = queryFromList
        [ "delimiter" =? lmuDelimiter
        , "encoding-type" =? lmuEncodingType
        , "key-marker" =? lmuKeyMarker
        , "max-uploads" =? lmuMaxUploads
        , "upload-id-marker" =? lmuUploadIdMarker
        , "uploads&prefix" =? lmuPrefix
        ]

instance ToHeaders ListMultipartUploads

instance AWSRequest ListMultipartUploads where
    type Er ListMultipartUploads = S3Error
    type Rs ListMultipartUploads = ListMultipartUploadsResponse
    request rq = s3 GET (service $ lmuBucket rq) rq
    response = receiveXML $ \hs doc -> ListMultipartUploadsResponse
        <$> xml "Bucket" doc
        <*> xml "CommonPrefixes" doc
        <*> hdr "Encoding-Type" hs
        <*> xml "IsTruncated" doc
        <*> xml "KeyMarker" doc
        <*> xml "MaxUploads" doc
        <*> xml "NextKeyMarker" doc
        <*> xml "NextUploadIdMarker" doc
        <*> xml "Prefix" doc
        <*> xml "UploadIdMarker" doc
        <*> xml "Uploads" doc

data ListMultipartUploadsResponse = ListMultipartUploadsResponse
    { lmurBucket :: Maybe Text
      -- ^ Name of the bucket to which the multipart upload was initiated.
    , lmurCommonPrefixes :: [CommonPrefixes]
    , lmurEncodingType :: Maybe Text
      -- ^ Encoding type used by Amazon S3 to encode object keys in the response.
    , lmurIsTruncated :: Maybe Bool
      -- ^ Indicates whether the returned list of multipart uploads is truncated. A
      -- value of true indicates that the list was truncated. The list can be
      -- truncated if the number of multipart uploads exceeds the limit allowed or
      -- specified by max uploads.
    , lmurKeyMarker :: Maybe Text
      -- ^ The key at or after which the listing began.
    , lmurMaxUploads :: Maybe Int
      -- ^ Maximum number of multipart uploads that could have been included in the
      -- response.
    , lmurNextKeyMarker :: Maybe Text
      -- ^ When a list is truncated, this element specifies the value that should be
      -- used for the key-marker request parameter in a subsequent request.
    , lmurNextUploadIdMarker :: Maybe Text
      -- ^ When a list is truncated, this element specifies the value that should be
      -- used for the upload-id-marker request parameter in a subsequent request.
    , lmurPrefix :: Maybe Text
      -- ^ When a prefix is provided in the request, this field contains the specified
      -- prefix. The result contains only keys starting with the specified prefix.
    , lmurUploadIdMarker :: Maybe Text
      -- ^ Upload ID after which listing began.
    , lmurUpload :: [Upload]
    } deriving (Eq, Show)