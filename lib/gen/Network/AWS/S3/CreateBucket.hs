{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.CreateBucket
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new bucket.
module Network.AWS.S3.CreateBucket where

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
createBucket :: Text
             -> AWS (Either S3Error CreateBucketResponse)
createBucket p1 = undefined $ CreateBucket
    { cbBucket = p1
    , cbACL = Nothing
    , cbCreateBucketConfiguration = Nothing
    , cbGrantFullControl = Nothing
    , cbGrantRead = Nothing
    , cbGrantReadACP = Nothing
    , cbGrantWrite = Nothing
    , cbGrantWriteACP = Nothing
    }

type PutBucket = CreateBucket
type PutBucketResponse = CreateBucketResponse

data CreateBucket = CreateBucket
    { cbACL :: Maybe ACL
      -- ^ The canned ACL to apply to the bucket.
    , cbBucket :: !Text
    , cbCreateBucketConfiguration :: Maybe CreateBucketConfiguration
    , cbGrantFullControl :: Maybe Text
      -- ^ Allows grantee the read, write, read ACP, and write ACP permissions on the
      -- bucket.
    , cbGrantRead :: Maybe Text
      -- ^ Allows grantee to list the objects in the bucket.
    , cbGrantReadACP :: Maybe Text
      -- ^ Allows grantee to read the bucket ACL.
    , cbGrantWrite :: Maybe Text
      -- ^ Allows grantee to create, overwrite, and delete any object in the bucket.
    , cbGrantWriteACP :: Maybe Text
      -- ^ Allows grantee to write the ACL for the applicable bucket.
    } deriving (Generic)

instance ToHeaders CreateBucket where
    toHeaders CreateBucket{..} =
        [ "x-amz-acl" =: cbACL
        , "x-amz-grant-full-control" =: cbGrantFullControl
        , "x-amz-grant-read" =: cbGrantRead
        , "x-amz-grant-read-acp" =: cbGrantReadACP
        , "x-amz-grant-write" =: cbGrantWrite
        , "x-amz-grant-write-acp" =: cbGrantWriteACP
        ]

instance ToPath CreateBucket where
    toPath CreateBucket{..} = Text.concat
        [ "/"
        , toText cbBucket
        ]

instance ToQuery CreateBucket where
    toQuery = const mempty

instance AWSRequest CreateBucket where
    type Er CreateBucket = S3Error
    type Rs CreateBucket = CreateBucketResponse
    request  = putS3 service
    response = undefined

data CreateBucketResponse = CreateBucketResponse
    { cbrsLocation :: Maybe Text
    } deriving (Eq, Show, Generic)
