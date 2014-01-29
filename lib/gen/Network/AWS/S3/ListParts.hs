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
listParts :: Text
          -> Text
          -> Text
          -> AWS (Either S3Error ListPartsResponse)
listParts p1 p2 p3 = undefined $ ListParts
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

instance ToHeaders ListParts

instance ToPath ListParts where
    toPath ListParts{..} = Text.concat
        [ "/"
        , toText lpBucket
        , "/"
        , toText lpKey
        ]

instance ToQuery ListParts where
    toQuery ListParts{..} = List
        [ "max-parts" =? lpMaxParts
        , "part-number-marker" =? lpPartNumberMarker
        , "uploadId" =? lpUploadId
        ]

instance AWSRequest ListParts where
    type Er ListParts = S3Error
    type Rs ListParts = ListPartsResponse
    request  = getS3 service
    response = undefined

data ListPartsResponse = ListPartsResponse
    { lprsBucket :: Maybe Text
      -- ^ Name of the bucket to which the multipart upload was initiated.
    , lprsInitiator :: Maybe Initiator
      -- ^ Identifies who initiated the multipart upload.
    , lprsIsTruncated :: Maybe Bool
      -- ^ Indicates whether the returned list of parts is truncated.
    , lprsKey :: Maybe Text
      -- ^ Object key for which the multipart upload was initiated.
    , lprsMaxParts :: Maybe Int
      -- ^ Maximum number of parts that were allowed in the response.
    , lprsNextPartNumberMarker :: Maybe Int
      -- ^ When a list is truncated, this element specifies the last part in the list,
      -- as well as the value to use for the part-number-marker request parameter in
      -- a subsequent request.
    , lprsOwner :: Maybe Owner
    , lprsPartNumberMarker :: Maybe Int
      -- ^ Part number after which listing begins.
    , lprsParts :: [Part]
    , lprsStorageClass :: Maybe StorageClass
      -- ^ The class of storage used to store the object.
    , lprsUploadId :: Maybe Text
      -- ^ Upload ID identifying the multipart upload whose parts are being listed.
    } deriving (Eq, Show, Generic)

instance FromXML ListPartsResponse where
    fromXMLOptions = xmlOptions
