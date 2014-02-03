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
completeMultipartUpload :: Text
                        -> Text
                        -> Text
                        -> CompleteMultipartUpload
completeMultipartUpload p1 p2 p3 = CompleteMultipartUpload
    { muBucket = p1
    , muKey = p2
    , muUploadId = p3
    , muMultipartUpload = Nothing
    }

data CompleteMultipartUpload = CompleteMultipartUpload
    { muBucket :: !Text
    , muKey :: !Text
    , muMultipartUpload :: Maybe MultipartUpload
    , muUploadId :: !Text
    } deriving (Generic)

instance ToHeaders CompleteMultipartUpload

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

instance AWSRequest CompleteMultipartUpload where
    type Er CompleteMultipartUpload = S3Error
    type Rs CompleteMultipartUpload = CompleteMultipartUploadResponse
    request  = postS3 service
    response = undefined

data CompleteMultipartUploadResponse = CompleteMultipartUploadResponse
    { mursBucket :: Maybe Text
    , mursETag :: Maybe Text
      -- ^ Entity tag of the object.
    , mursExpiration :: Maybe UTCTime
      -- ^ If the object expiration is configured, this will contain the expiration
      -- date (expiry-date) and rule ID (rule-id). The value of rule-id is URL
      -- encoded.
    , mursKey :: Maybe Text
    , mursLocation :: Maybe Text
    , mursServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this object in S3.
    , mursVersionId :: Maybe Text
      -- ^ Version of the object.
    } deriving (Eq, Show, Generic)

instance FromXML CompleteMultipartUploadResponse where
    fromXMLOptions = xmlOptions
