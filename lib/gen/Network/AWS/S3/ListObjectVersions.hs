{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.ListObjectVersions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns metadata about all of the versions of objects in a bucket.
module Network.AWS.S3.ListObjectVersions where

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
listObjectVersions :: Text
                   -> ListObjectVersions
listObjectVersions p1 = ListObjectVersions
    { lovBucket = p1
    , lovDelimiter = Nothing
    , lovEncodingType = Nothing
    , lovKeyMarker = Nothing
    , lovMaxKeys = Nothing
    , lovPrefix = Nothing
    , lovVersionIdMarker = Nothing
    }

type GetBucketObjectVersions = ListObjectVersions
type GetBucketObjectVersionsResponse = ListObjectVersionsResponse

data ListObjectVersions = ListObjectVersions
    { lovBucket :: !Text
    , lovDelimiter :: Maybe Text
      -- ^ A delimiter is a character you use to group keys.
    , lovEncodingType :: Maybe EncodingType
      -- ^ Requests Amazon S3 to encode the object keys in the response and specifies
      -- the encoding method to use. An object key may contain any Unicode
      -- character; however, XML 1.0 parser cannot parse some characters, such as
      -- characters with an ASCII value from 0 to 10. For characters that are not
      -- supported in XML 1.0, you can add this parameter to request that Amazon S3
      -- encode the keys in the response.
    , lovKeyMarker :: Maybe Text
      -- ^ Specifies the key to start with when listing objects in a bucket.
    , lovMaxKeys :: Maybe Int
      -- ^ Sets the maximum number of keys returned in the response. The response
      -- might contain fewer keys but will never contain more.
    , lovPrefix :: Maybe Text
      -- ^ Limits the response to keys that begin with the specified prefix.
    , lovVersionIdMarker :: Maybe Text
      -- ^ Specifies the object version you want to start listing from.
    } deriving (Generic)

instance ToPath ListObjectVersions where
    toPath ListObjectVersions{..} = Text.concat
        [ "/"
        , toText lovBucket
        ]

instance ToQuery ListObjectVersions where
    toQuery ListObjectVersions{..} = queryFromList
        [ "encoding-type" =? lovEncodingType
        , "key-marker" =? lovKeyMarker
        , "max-keys" =? lovMaxKeys
        , "prefix" =? lovPrefix
        , "version-id-marker" =? lovVersionIdMarker
        , "versions&delimiter" =? lovDelimiter
        ]

instance ToHeaders ListObjectVersions

instance AWSRequest ListObjectVersions where
    type Er ListObjectVersions = S3Error
    type Rs ListObjectVersions = ListObjectVersionsResponse
    request rq = s3 GET (service $ lovBucket rq) rq
    response = receiveXML $ \hs doc -> ListObjectVersionsResponse
        <$> xml "CommonPrefixes" doc
        <*> xml "DeleteMarkers" doc
        <*> hdr "Encoding-Type" hs
        <*> xml "IsTruncated" doc
        <*> xml "KeyMarker" doc
        <*> xml "MaxKeys" doc
        <*> xml "Name" doc
        <*> xml "NextKeyMarker" doc
        <*> xml "NextVersionIdMarker" doc
        <*> xml "Prefix" doc
        <*> xml "VersionIdMarker" doc
        <*> xml "Versions" doc

data ListObjectVersionsResponse = ListObjectVersionsResponse
    { lovrCommonPrefixes :: [CommonPrefixes]
    , lovrDeleteMarkers :: [DeleteMarker]
    , lovrEncodingType :: Maybe Text
      -- ^ Encoding type used by Amazon S3 to encode object keys in the response.
    , lovrIsTruncated :: Maybe Bool
      -- ^ A flag that indicates whether or not Amazon S3 returned all of the results
      -- that satisfied the search criteria. If your results were truncated, you can
      -- make a follow-up paginated request using the NextKeyMarker and
      -- NextVersionIdMarker response parameters as a starting place in another
      -- request to return the rest of the results.
    , lovrKeyMarker :: Maybe Text
      -- ^ Marks the last Key returned in a truncated response.
    , lovrMaxKeys :: Maybe Int
    , lovrName :: Maybe Text
    , lovrNextKeyMarker :: Maybe Text
      -- ^ Use this value for the key marker request parameter in a subsequent
      -- request.
    , lovrNextVersionIdMarker :: Maybe Text
      -- ^ Use this value for the next version id marker parameter in a subsequent
      -- request.
    , lovrPrefix :: Maybe Text
    , lovrVersionIdMarker :: Maybe Text
    , lovrVersions :: [Version]
    } deriving (Eq, Show)
