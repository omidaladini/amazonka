{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketLifecycle
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the lifecycle configuration information set on the bucket.
module Network.AWS.S3.GetBucketLifecycle where

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
getBucketLifecycle :: Text
                   -> GetBucketLifecycle
getBucketLifecycle p1 = undefined $ GetBucketLifecycle
    { gbmBucket = p1
    }

data GetBucketLifecycle = GetBucketLifecycle
    { gbmBucket :: !Text
    } deriving (Generic)

instance ToHeaders GetBucketLifecycle

instance ToPath GetBucketLifecycle where
    toPath GetBucketLifecycle{..} = Text.concat
        [ "/"
        , toText gbmBucket
        ]

instance ToQuery GetBucketLifecycle where
    toQuery GetBucketLifecycle{..} = List
        [ "lifecycle"
        ]

instance AWSRequest GetBucketLifecycle where
    type Er GetBucketLifecycle = S3Error
    type Rs GetBucketLifecycle = GetBucketLifecycleResponse
    request  = getS3 service
    response = undefined

data GetBucketLifecycleResponse = GetBucketLifecycleResponse
    { gbmrsRules :: [Rule]
    } deriving (Eq, Show, Generic)

instance FromXML GetBucketLifecycleResponse where
    fromXMLOptions = xmlOptions
