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
listObjectVersions :: Text
                   -> AWS (Either S3Error ListObjectVersionsResponse)
listObjectVersions p1 = undefined $ ListObjectVersions
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

instance ToHeaders ListObjectVersions

instance ToPath ListObjectVersions where
    toPath ListObjectVersions{..} = Text.concat
        [ "/"
        , toText lovBucket
        ]

instance ToQuery ListObjectVersions where
    toQuery ListObjectVersions{..} = List
        [ "encoding-type" =? lovEncodingType
        , "key-marker" =? lovKeyMarker
        , "max-keys" =? lovMaxKeys
        , "prefix" =? lovPrefix
        , "version-id-marker" =? lovVersionIdMarker
        , "versions&delimiter" =? lovDelimiter
        ]

instance AWSRequest ListObjectVersions where
    type Er ListObjectVersions = S3Error
    type Rs ListObjectVersions = ListObjectVersionsResponse
    request  = getS3 service
    response = undefined

data ListObjectVersionsResponse = ListObjectVersionsResponse
    { lovrsCommonPrefixes :: [CommonPrefixes]
    , lovrsDeleteMarkers :: [DeleteMarker]
    , lovrsEncodingType :: Maybe Text
      -- ^ Encoding type used by Amazon S3 to encode object keys in the response.
    , lovrsIsTruncated :: Maybe Bool
      -- ^ A flag that indicates whether or not Amazon S3 returned all of the results
      -- that satisfied the search criteria. If your results were truncated, you can
      -- make a follow-up paginated request using the NextKeyMarker and
      -- NextVersionIdMarker response parameters as a starting place in another
      -- request to return the rest of the results.
    , lovrsKeyMarker :: Maybe Text
      -- ^ Marks the last Key returned in a truncated response.
    , lovrsMaxKeys :: Maybe Int
    , lovrsName :: Maybe Text
    , lovrsNextKeyMarker :: Maybe Text
      -- ^ Use this value for the key marker request parameter in a subsequent
      -- request.
    , lovrsNextVersionIdMarker :: Maybe Text
      -- ^ Use this value for the next version id marker parameter in a subsequent
      -- request.
    , lovrsPrefix :: Maybe Text
    , lovrsVersionIdMarker :: Maybe Text
    , lovrsVersions :: [Version]
    } deriving (Eq, Show, Generic)

instance FromXML ListObjectVersionsResponse where
    fromXMLOptions = xmlOptions
