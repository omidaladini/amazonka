{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.UploadPart
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Uploads a part in a multipart upload.Note: After you initiate multipart
-- upload and upload one or more parts, you must either complete or abort
-- multipart upload in order to stop getting charged for storage of the
-- uploaded parts. Only after you either complete or abort multipart upload,
-- Amazon S3 frees up the parts storage and stops charging you for the parts
-- storage.
module Network.AWS.S3.UploadPart where

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
uploadPart :: Text
           -> Text
           -> RequestBody
           -> Int
           -> Text
           -> AWS (Either S3Error UploadPartResponse)
uploadPart p1 p2 p3 p4 p5 = undefined $ UploadPart
    { upBucket = p1
    , upKey = p2
    , upBody = p3
    , upPartNumber = p4
    , upUploadId = p5
    , upContentLength = Nothing
    , upContentMD5 = Nothing
    }

data UploadPart = UploadPart
    { upBody :: RequestBody
    , upBucket :: !Text
    , upContentLength :: Maybe Int
      -- ^ Size of the body in bytes. This parameter is useful when the size of the
      -- body cannot be determined automatically.
    , upContentMD5 :: Maybe Text
    , upKey :: !Text
    , upPartNumber :: !Int
      -- ^ Part number of part being uploaded.
    , upUploadId :: !Text
      -- ^ Upload ID identifying the multipart upload whose part is being uploaded.
    } deriving (Generic)

instance ToHeaders UploadPart where
    toHeaders UploadPart{..} =
        [ "Content-Length" =: upContentLength
        , "Content-MD5" =: upContentMD5
        ]

instance ToPath UploadPart where
    toPath UploadPart{..} = Text.concat
        [ "/"
        , toText upBucket
        , "/"
        , toText upKey
        ]

instance ToQuery UploadPart where
    toQuery UploadPart{..} = List
        [ "partNumber" =? upPartNumber
        , "uploadId" =? upUploadId
        ]

instance AWSRequest UploadPart where
    type Er UploadPart = S3Error
    type Rs UploadPart = UploadPartResponse
    request  = putS3 service
    response = undefined

data UploadPartResponse = UploadPartResponse
    { uprsETag :: Maybe Text
      -- ^ Entity tag for the uploaded object.
    , uprsServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this object in S3.
    } deriving (Eq, Show, Generic)
