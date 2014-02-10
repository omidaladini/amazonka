{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.DeleteObject
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes the null version (if there is one) of an object and inserts a
-- delete marker, which becomes the latest version of the object. If there
-- isn't a null version, Amazon S3 does not remove any objects.
module Network.AWS.S3.DeleteObject where

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteObject :: Text
             -> Text
             -> DeleteObject
deleteObject p1 p2 = DeleteObject
    { doBucket = p1
    , doKey = p2
    , doMFA = Nothing
    , doVersionId = Nothing
    }

data DeleteObject = DeleteObject
    { doBucket :: !Text
    , doKey :: !Text
    , doMFA :: Maybe Text
      -- ^ The concatenation of the authentication device's serial number, a space,
      -- and the value that is displayed on your authentication device.
    , doVersionId :: Maybe Text
      -- ^ VersionId used to reference a specific version of the object.
    } deriving (Generic)

instance ToPath DeleteObject where
    toPath DeleteObject{..} = Text.concat
        [ "/"
        , toText doBucket
        , "/"
        , toText doKey
        ]

instance ToQuery DeleteObject where
    toQuery DeleteObject{..} = queryFromList
        [ "versionId" =? doVersionId
        ]

instance ToHeaders DeleteObject where
    toHeaders DeleteObject{..} =
        [ "x-amz-mfa" =: doMFA
        ]

instance AWSRequest DeleteObject where
    type Er DeleteObject = S3Error
    type Rs DeleteObject = DeleteObjectResponse
    request rq = s3 DELETE (service $ doBucket rq) rq
    response = receiveHeaders $ \hs -> DeleteObjectResponse
        <$> hdr "x-amz-delete-marker" hs
        <*> hdr "x-amz-version-id" hs

data DeleteObjectResponse = DeleteObjectResponse
    { dorDeleteMarker :: Maybe Bool
      -- ^ Specifies whether the versioned object that was permanently deleted was
      -- (true) or was not (false) a delete marker.
    , dorVersionId :: Maybe Text
      -- ^ Returns the version ID of the delete marker created as a result of the
      -- DELETE operation.
    } deriving (Eq, Show)
