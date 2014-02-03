{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.ListObjects
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns some or all (up to 1000) of the objects in a bucket. You can use
-- the request parameters as selection criteria to return a subset of the
-- objects in a bucket.
module Network.AWS.S3.ListObjects where

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
listObjects :: Text
            -> ListObjects
listObjects p1 = ListObjects
    { loBucket = p1
    , loDelimiter = Nothing
    , loEncodingType = Nothing
    , loMarker = Nothing
    , loMaxKeys = Nothing
    , loPrefix = Nothing
    }

type GetBucket = ListObjects
type GetBucketResponse = ListObjectsResponse

data ListObjects = ListObjects
    { loBucket :: !Text
    , loDelimiter :: Maybe Text
      -- ^ A delimiter is a character you use to group keys.
    , loEncodingType :: Maybe EncodingType
      -- ^ Requests Amazon S3 to encode the object keys in the response and specifies
      -- the encoding method to use. An object key may contain any Unicode
      -- character; however, XML 1.0 parser cannot parse some characters, such as
      -- characters with an ASCII value from 0 to 10. For characters that are not
      -- supported in XML 1.0, you can add this parameter to request that Amazon S3
      -- encode the keys in the response.
    , loMarker :: Maybe Text
      -- ^ Specifies the key to start with when listing objects in a bucket.
    , loMaxKeys :: Maybe Int
      -- ^ Sets the maximum number of keys returned in the response. The response
      -- might contain fewer keys but will never contain more.
    , loPrefix :: Maybe Text
      -- ^ Limits the response to keys that begin with the specified prefix.
    } deriving (Generic)

instance ToHeaders ListObjects

instance ToPath ListObjects where
    toPath ListObjects{..} = Text.concat
        [ "/"
        , toText loBucket
        ]

instance ToQuery ListObjects where
    toQuery ListObjects{..} = queryFromList
        [ "delimiter" =? loDelimiter
        , "encoding-type" =? loEncodingType
        , "marker" =? loMarker
        , "max-keys" =? loMaxKeys
        , "prefix" =? loPrefix
        ]

instance AWSRequest ListObjects where
    type Er ListObjects = S3Error
    type Rs ListObjects = ListObjectsResponse
    request  = getS3 service
    response = undefined

data ListObjectsResponse = ListObjectsResponse
    { lorsCommonPrefixes :: [CommonPrefixes]
    , lorsContents :: [Contents]
    , lorsEncodingType :: Maybe Text
      -- ^ Encoding type used by Amazon S3 to encode object keys in the response.
    , lorsIsTruncated :: Maybe Bool
      -- ^ A flag that indicates whether or not Amazon S3 returned all of the results
      -- that satisfied the search criteria.
    , lorsMarker :: Maybe Text
    , lorsMaxKeys :: Maybe Int
    , lorsName :: Maybe Text
    , lorsNextMarker :: Maybe Text
      -- ^ When response is truncated (the IsTruncated element value in the response
      -- is true), you can use the key name in this field as marker in the
      -- subsequent request to get next set of objects. Amazon S3 lists objects in
      -- alphabetical order Note: This element is returned only if you have
      -- delimiter request parameter specified. If response does not include the
      -- NextMaker and it is truncated, you can use the value of the last Key in the
      -- response as the marker in the subsequent request to get the next set of
      -- object keys.
    , lorsPrefix :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromXML ListObjectsResponse where
    fromXMLOptions = xmlOptions
