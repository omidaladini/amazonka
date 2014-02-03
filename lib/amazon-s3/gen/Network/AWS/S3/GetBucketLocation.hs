{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketLocation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the region the bucket resides in.
module Network.AWS.S3.GetBucketLocation where

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
getBucketLocation :: Text
                  -> GetBucketLocation
getBucketLocation p1 = GetBucketLocation
    { gblBucket = p1
    }

data GetBucketLocation = GetBucketLocation
    { gblBucket :: !Text
    } deriving (Generic)

instance ToHeaders GetBucketLocation

instance ToPath GetBucketLocation where
    toPath GetBucketLocation{..} = Text.concat
        [ "/"
        , toText gblBucket
        ]

instance ToQuery GetBucketLocation where
    toQuery GetBucketLocation{..} = queryFromList
        [ "location"
        ]

instance AWSRequest GetBucketLocation where
    type Er GetBucketLocation = S3Error
    type Rs GetBucketLocation = GetBucketLocationResponse
    request  = getS3 service
    response = undefined

data GetBucketLocationResponse = GetBucketLocationResponse
    { gblrsLocationConstraint :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromXML GetBucketLocationResponse where
    fromXMLOptions = xmlOptions
