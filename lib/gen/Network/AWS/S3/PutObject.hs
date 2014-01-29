{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutObject
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds an object to a bucket.
module Network.AWS.S3.PutObject where

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
putObject :: Text
          -> Text
          -> RequestBody
          -> PutObject
putObject p1 p2 p3 = undefined $ PutObject
    { poBucket = p1
    , poKey = p2
    , poBody = p3
    , poACL = Nothing
    , poCacheControl = Nothing
    , poContentDisposition = Nothing
    , poContentEncoding = Nothing
    , poContentLanguage = Nothing
    , poContentLength = Nothing
    , poContentMD5 = Nothing
    , poContentType = Nothing
    , poExpires = Nothing
    , poGrantFullControl = Nothing
    , poGrantRead = Nothing
    , poGrantReadACP = Nothing
    , poGrantWriteACP = Nothing
    , poMetadata = Map.empty
    , poServerSideEncryption = Nothing
    , poStorageClass = Nothing
    , poWebsiteRedirectLocation = Nothing
    }

data PutObject = PutObject
    { poACL :: Maybe ACL
      -- ^ The canned ACL to apply to the object.
    , poBody :: RequestBody
    , poBucket :: !Text
    , poCacheControl :: Maybe Text
      -- ^ Specifies caching behavior along the request/reply chain.
    , poContentDisposition :: Maybe Text
      -- ^ Specifies presentational information for the object.
    , poContentEncoding :: Maybe Text
      -- ^ Specifies what content encodings have been applied to the object and thus
      -- what decoding mechanisms must be applied to obtain the media-type
      -- referenced by the Content-Type header field.
    , poContentLanguage :: Maybe Text
      -- ^ The language the content is in.
    , poContentLength :: Maybe Int
      -- ^ Size of the body in bytes. This parameter is useful when the size of the
      -- body cannot be determined automatically.
    , poContentMD5 :: Maybe Text
    , poContentType :: Maybe Text
      -- ^ A standard MIME type describing the format of the object data.
    , poExpires :: Maybe UTCTime
      -- ^ The date and time at which the object is no longer cacheable.
    , poGrantFullControl :: Maybe Text
      -- ^ Gives the grantee READ, READ_ACP, and WRITE_ACP permissions on the object.
    , poGrantRead :: Maybe Text
      -- ^ Allows grantee to read the object data and its metadata.
    , poGrantReadACP :: Maybe Text
      -- ^ Allows grantee to read the object ACL.
    , poGrantWriteACP :: Maybe Text
      -- ^ Allows grantee to write the ACL for the applicable object.
    , poKey :: !Text
    , poMetadata :: HashMap Text Text
      -- ^ A map of metadata to store with the object in S3.
    , poServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this object in S3.
    , poStorageClass :: Maybe StorageClass
      -- ^ The type of storage to use for the object. Defaults to 'STANDARD'.
    , poWebsiteRedirectLocation :: Maybe Text
      -- ^ If the bucket is configured as a website, redirects requests for this
      -- object to another object in the same bucket or to an external URL. Amazon
      -- S3 stores the value of this header in the object metadata.
    } deriving (Generic)

instance ToHeaders PutObject where
    toHeaders PutObject{..} =
        [ "x-amz-acl" =: poACL
        , "Cache-Control" =: poCacheControl
        , "Content-Disposition" =: poContentDisposition
        , "Content-Encoding" =: poContentEncoding
        , "Content-Language" =: poContentLanguage
        , "Content-Length" =: poContentLength
        , "Content-MD5" =: poContentMD5
        , "Content-Type" =: poContentType
        , "Expires" =: poExpires
        , "x-amz-grant-full-control" =: poGrantFullControl
        , "x-amz-grant-read" =: poGrantRead
        , "x-amz-grant-read-acp" =: poGrantReadACP
        , "x-amz-grant-write-acp" =: poGrantWriteACP
        , "x-amz-server-side-encryption" =: poServerSideEncryption
        , "x-amz-storage-class" =: poStorageClass
        , "x-amz-website-redirect-location" =: poWebsiteRedirectLocation
        ]

instance ToPath PutObject where
    toPath PutObject{..} = Text.concat
        [ "/"
        , toText poBucket
        , "/"
        , toText poKey
        ]

instance ToQuery PutObject where
    toQuery = const mempty

instance AWSRequest PutObject where
    type Er PutObject = S3Error
    type Rs PutObject = PutObjectResponse
    request  = putS3 service
    response = undefined

data PutObjectResponse = PutObjectResponse
    { porsETag :: Maybe Text
      -- ^ Entity tag for the uploaded object.
    , porsExpiration :: Maybe UTCTime
      -- ^ If the object expiration is configured, this will contain the expiration
      -- date (expiry-date) and rule ID (rule-id). The value of rule-id is URL
      -- encoded.
    , porsServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this object in S3.
    , porsVersionId :: Maybe Text
      -- ^ Version of the object.
    } deriving (Eq, Show, Generic)
