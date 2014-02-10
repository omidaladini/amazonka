{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.AbortMultipartUpload
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Aborts a multipart upload.To verify that all parts have been removed, so
-- you don't get charged for the part storage, you should call the List Parts
-- operation and ensure the parts list is empty.
module Network.AWS.S3.AbortMultipartUpload where

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

data AbortMultipartUpload = AbortMultipartUpload
    { amuBucket :: !Text
    , amuKey :: !Text
    , amuUploadId :: !Text
    } deriving (Generic)

instance ToPath AbortMultipartUpload where
    toPath AbortMultipartUpload{..} = Text.concat
        [ "/"
        , toText amuBucket
        , "/"
        , toText amuKey
        ]

instance ToQuery AbortMultipartUpload where
    toQuery AbortMultipartUpload{..} = queryFromList
        [ "uploadId" =? amuUploadId
        ]

instance ToHeaders AbortMultipartUpload

instance AWSRequest AbortMultipartUpload where
    type Er AbortMultipartUpload = S3Error
    type Rs AbortMultipartUpload = AbortMultipartUploadResponse
    request rq = s3 DELETE (service $ amuBucket rq) rq
    response = receiveEmpty AbortMultipartUploadResponse

data AbortMultipartUploadResponse = AbortMultipartUploadResponse
   deriving (Eq, Show)
