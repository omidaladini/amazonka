{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.ListParts
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the parts that have been uploaded for a specific multipart upload.
module Network.AWS.S3.ListParts where

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
listParts :: Text
          -> Text
          -> Text
          -- ^ Upload ID identifying the multipart upload whose parts are being listed.
          -> ListParts
listParts p1 p2 p3 = ListParts
    { lpBucket = p1
    , lpKey = p2
    , lpUploadId = p3
    , lpMaxParts = Nothing
    , lpPartNumberMarker = Nothing
    }

data ListParts = ListParts
    { lpBucket :: !Text
    , lpKey :: !Text
    , lpMaxParts :: Maybe Int
      -- ^ Sets the maximum number of parts to return.
    , lpPartNumberMarker :: Maybe Int
      -- ^ Specifies the part after which listing should begin. Only parts with higher
      -- part numbers will be listed.
    , lpUploadId :: !Text
      -- ^ Upload ID identifying the multipart upload whose parts are being listed.
    } deriving (Generic)

instance ToPath ListParts where
    toPath ListParts{..} = Text.concat
        [ "/"
        , toText lpBucket
        , "/"
        , toText lpKey
        ]

instance ToQuery ListParts where
    toQuery ListParts{..} = queryFromList
        [ "max-parts" =? lpMaxParts
        , "part-number-marker" =? lpPartNumberMarker
        , "uploadId" =? lpUploadId
        ]

instance ToHeaders ListParts

instance AWSRequest ListParts where
    type Er ListParts = S3Error
    type Rs ListParts = ListPartsResponse
    request rq = s3 GET (service $ lpBucket rq) rq
    response = receiveXML $ \hs doc -> ListPartsResponse
        <$> xml "Bucket" doc
        <*> xml "Initiator" doc
        <*> xml "IsTruncated" doc
        <*> xml "Key" doc
        <*> xml "MaxParts" doc
        <*> xml "NextPartNumberMarker" doc
        <*> xml "Owner" doc
        <*> xml "PartNumberMarker" doc
        <*> xml "Parts" doc
        <*> xml "StorageClass" doc
        <*> xml "UploadId" doc

data ListPartsResponse = ListPartsResponse
    { lprBucket :: Maybe Text
      -- ^ Name of the bucket to which the multipart upload was initiated.
    , lprInitiator :: Maybe Initiator
      -- ^ Identifies who initiated the multipart upload.
    , lprIsTruncated :: Maybe Bool
      -- ^ Indicates whether the returned list of parts is truncated.
    , lprKey :: Maybe Text
      -- ^ Object key for which the multipart upload was initiated.
    , lprMaxParts :: Maybe Int
      -- ^ Maximum number of parts that were allowed in the response.
    , lprNextPartNumberMarker :: Maybe Int
      -- ^ When a list is truncated, this element specifies the last part in the list,
      -- as well as the value to use for the part-number-marker request parameter in
      -- a subsequent request.
    , lprOwner :: Maybe Owner
    , lprPartNumberMarker :: Maybe Int
      -- ^ Part number after which listing begins.
    , lprParts :: [Part]
    , lprStorageClass :: Maybe StorageClass
      -- ^ The class of storage used to store the object.
    , lprUploadId :: Maybe Text
      -- ^ Upload ID identifying the multipart upload whose parts are being listed.
    } deriving (Eq, Show)
