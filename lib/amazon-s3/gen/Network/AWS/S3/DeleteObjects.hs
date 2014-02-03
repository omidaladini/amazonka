{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.DeleteObjects
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation enables you to delete multiple objects from a bucket using a
-- single HTTP request. You may specify up to 1000 keys.
module Network.AWS.S3.DeleteObjects where

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
deleteObjects :: Text
              -> Delete
              -> DeleteObjects
deleteObjects p1 p2 = DeleteObjects
    { dpBucket = p1
    , dpDelete = p2
    , dpMFA = Nothing
    }

type DeleteMultipleObjects = DeleteObjects
type DeleteMultipleObjectsResponse = DeleteObjectsResponse

data DeleteObjects = DeleteObjects
    { dpBucket :: !Text
    , dpDelete :: Delete
    , dpMFA :: Maybe Text
      -- ^ The concatenation of the authentication device's serial number, a space,
      -- and the value that is displayed on your authentication device.
    } deriving (Generic)

instance ToHeaders DeleteObjects where
    toHeaders DeleteObjects{..} =
        [ "x-amz-mfa" =: dpMFA
        ]

instance ToPath DeleteObjects where
    toPath DeleteObjects{..} = Text.concat
        [ "/"
        , toText dpBucket
        ]

instance ToQuery DeleteObjects where
    toQuery DeleteObjects{..} = queryFromList
        [ "delete"
        ]

instance AWSRequest DeleteObjects where
    type Er DeleteObjects = S3Error
    type Rs DeleteObjects = DeleteObjectsResponse
    request  = postS3 service
    response = undefined

data DeleteObjectsResponse = DeleteObjectsResponse
    { dprsDeleted :: [Deleted]
    , dprsErrors :: [Error]
    } deriving (Eq, Show, Generic)

instance FromXML DeleteObjectsResponse where
    fromXMLOptions = xmlOptions
