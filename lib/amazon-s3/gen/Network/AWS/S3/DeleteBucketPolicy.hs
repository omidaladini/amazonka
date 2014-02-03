{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.DeleteBucketPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the policy from the bucket.
module Network.AWS.S3.DeleteBucketPolicy where

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
deleteBucketPolicy :: Text
                   -> DeleteBucketPolicy
deleteBucketPolicy p1 = DeleteBucketPolicy
    { dbpBucket = p1
    }

data DeleteBucketPolicy = DeleteBucketPolicy
    { dbpBucket :: !Text
    } deriving (Generic)

instance ToHeaders DeleteBucketPolicy

instance ToPath DeleteBucketPolicy where
    toPath DeleteBucketPolicy{..} = Text.concat
        [ "/"
        , toText dbpBucket
        ]

instance ToQuery DeleteBucketPolicy where
    toQuery DeleteBucketPolicy{..} = queryFromList
        [ "policy"
        ]

instance AWSRequest DeleteBucketPolicy where
    type Er DeleteBucketPolicy = S3Error
    type Rs DeleteBucketPolicy = DeleteBucketPolicyResponse
    request  = deleteS3 service
    response = undefined

data DeleteBucketPolicyResponse = DeleteBucketPolicyResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteBucketPolicyResponse where
    fromXMLOptions = xmlOptions
