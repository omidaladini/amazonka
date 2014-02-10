{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.PutObjectAcl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | uses the acl subresource to set the access control list (ACL) permissions
-- for an object that already exists in a bucket.
module Network.AWS.S3.PutObjectAcl where

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
putObjectAcl :: Text
             -> Text
             -> PutObjectAcl
putObjectAcl p1 p2 = PutObjectAcl
    { poaBucket = p1
    , poaKey = p2
    , poaACL = Nothing
    , poaAccessControlPolicy = Nothing
    , poaContentMD5 = Nothing
    , poaGrantFullControl = Nothing
    , poaGrantRead = Nothing
    , poaGrantReadACP = Nothing
    , poaGrantWrite = Nothing
    , poaGrantWriteACP = Nothing
    }

data PutObjectAcl = PutObjectAcl
    { poaACL :: Maybe ACL
      -- ^ The canned ACL to apply to the bucket.
    , poaAccessControlPolicy :: Maybe AccessControlPolicy
    , poaBucket :: !Text
    , poaContentMD5 :: Maybe Text
    , poaGrantFullControl :: Maybe Text
      -- ^ Allows grantee the read, write, read ACP, and write ACP permissions on the
      -- bucket.
    , poaGrantRead :: Maybe Text
      -- ^ Allows grantee to list the objects in the bucket.
    , poaGrantReadACP :: Maybe Text
      -- ^ Allows grantee to read the bucket ACL.
    , poaGrantWrite :: Maybe Text
      -- ^ Allows grantee to create, overwrite, and delete any object in the bucket.
    , poaGrantWriteACP :: Maybe Text
      -- ^ Allows grantee to write the ACL for the applicable bucket.
    , poaKey :: !Text
    } deriving (Generic)

instance ToPath PutObjectAcl where
    toPath PutObjectAcl{..} = Text.concat
        [ "/"
        , toText poaBucket
        , "/"
        , toText poaKey
        ]

instance ToQuery PutObjectAcl where
    toQuery PutObjectAcl{..} = queryFromList
        [ "acl"
        ]

instance ToHeaders PutObjectAcl where
    toHeaders PutObjectAcl{..} =
        [ "x-amz-acl" =: poaACL
        , "Content-MD5" =: poaContentMD5
        , "x-amz-grant-full-control" =: poaGrantFullControl
        , "x-amz-grant-read" =: poaGrantRead
        , "x-amz-grant-read-acp" =: poaGrantReadACP
        , "x-amz-grant-write" =: poaGrantWrite
        , "x-amz-grant-write-acp" =: poaGrantWriteACP
        ]

instance AWSRequest PutObjectAcl where
    type Er PutObjectAcl = S3Error
    type Rs PutObjectAcl = PutObjectAclResponse
    request rq = s3XML PUT (service $ poaBucket rq) (poaAccessControlPolicy rq) rq
    response = receiveEmpty PutObjectAclResponse

data PutObjectAclResponse = PutObjectAclResponse
   deriving (Eq, Show)