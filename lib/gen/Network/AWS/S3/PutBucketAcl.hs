{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutBucketAcl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the permissions on a bucket using access control lists (ACL).
module Network.AWS.S3.PutBucketAcl where

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
putBucketAcl :: Text
             -> PutBucketAcl
putBucketAcl p1 = undefined $ PutBucketAcl
    { pbaBucket = p1
    , pbaACL = Nothing
    , pbaAccessControlPolicy = Nothing
    , pbaContentMD5 = Nothing
    , pbaGrantFullControl = Nothing
    , pbaGrantRead = Nothing
    , pbaGrantReadACP = Nothing
    , pbaGrantWrite = Nothing
    , pbaGrantWriteACP = Nothing
    }

data PutBucketAcl = PutBucketAcl
    { pbaACL :: Maybe ACL
      -- ^ The canned ACL to apply to the bucket.
    , pbaAccessControlPolicy :: Maybe AccessControlPolicy
    , pbaBucket :: !Text
    , pbaContentMD5 :: Maybe Text
    , pbaGrantFullControl :: Maybe Text
      -- ^ Allows grantee the read, write, read ACP, and write ACP permissions on the
      -- bucket.
    , pbaGrantRead :: Maybe Text
      -- ^ Allows grantee to list the objects in the bucket.
    , pbaGrantReadACP :: Maybe Text
      -- ^ Allows grantee to read the bucket ACL.
    , pbaGrantWrite :: Maybe Text
      -- ^ Allows grantee to create, overwrite, and delete any object in the bucket.
    , pbaGrantWriteACP :: Maybe Text
      -- ^ Allows grantee to write the ACL for the applicable bucket.
    } deriving (Generic)

instance ToHeaders PutBucketAcl where
    toHeaders PutBucketAcl{..} =
        [ "x-amz-acl" =: pbaACL
        , "Content-MD5" =: pbaContentMD5
        , "x-amz-grant-full-control" =: pbaGrantFullControl
        , "x-amz-grant-read" =: pbaGrantRead
        , "x-amz-grant-read-acp" =: pbaGrantReadACP
        , "x-amz-grant-write" =: pbaGrantWrite
        , "x-amz-grant-write-acp" =: pbaGrantWriteACP
        ]

instance ToPath PutBucketAcl where
    toPath PutBucketAcl{..} = Text.concat
        [ "/"
        , toText pbaBucket
        ]

instance ToQuery PutBucketAcl where
    toQuery PutBucketAcl{..} = List
        [ "acl"
        ]

instance AWSRequest PutBucketAcl where
    type Er PutBucketAcl = S3Error
    type Rs PutBucketAcl = PutBucketAclResponse
    request  = putS3 service
    response = undefined

data PutBucketAclResponse = PutBucketAclResponse
    deriving (Eq, Show, Generic)

instance FromXML PutBucketAclResponse where
    fromXMLOptions = xmlOptions
