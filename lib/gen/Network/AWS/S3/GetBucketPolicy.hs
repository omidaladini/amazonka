{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the policy of a specified bucket.
module Network.AWS.S3.GetBucketPolicy where

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

data GetBucketPolicy = GetBucketPolicy
    { gbpBucket :: !Text
    } deriving (Generic)

instance ToHeaders GetBucketPolicy

instance ToPath GetBucketPolicy where
    toPath GetBucketPolicy{..} = Text.concat
        [ "/"
        , toText gbpBucket
        ]

instance ToQuery GetBucketPolicy where
    toQuery GetBucketPolicy{..} = List
        [ "policy"
        ]

instance AWSRequest GetBucketPolicy where
    type Er GetBucketPolicy = S3Error
    type Rs GetBucketPolicy = GetBucketPolicyResponse
    request  = getS3 service
    response = undefined

data GetBucketPolicyResponse = GetBucketPolicyResponse
    { gbprsPolicy :: Maybe Text
      -- ^ The bucket policy as a JSON document.
    } deriving (Eq, Show, Generic)

instance FromXML GetBucketPolicyResponse where
    fromXMLOptions = xmlOptions
