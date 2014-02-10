{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.RestoreObject
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Restores an archived copy of an object back into Amazon S3.
module Network.AWS.S3.RestoreObject where

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
restoreObject :: Text
              -> Text
              -> RestoreObject
restoreObject p1 p2 = RestoreObject
    { roBucket = p1
    , roKey = p2
    , roRestoreRequest = Nothing
    }

type PostObjectRestore = RestoreObject
type PostObjectRestoreResponse = RestoreObjectResponse

data RestoreObject = RestoreObject
    { roBucket :: !Text
    , roKey :: !Text
    , roRestoreRequest :: Maybe RestoreRequest
    } deriving (Generic)

instance ToPath RestoreObject where
    toPath RestoreObject{..} = Text.concat
        [ "/"
        , toText roBucket
        , "/"
        , toText roKey
        ]

instance ToQuery RestoreObject where
    toQuery RestoreObject{..} = queryFromList
        [ "restore"
        ]

instance ToHeaders RestoreObject

instance AWSRequest RestoreObject where
    type Er RestoreObject = S3Error
    type Rs RestoreObject = RestoreObjectResponse
    request rq = s3XML POST (service $ roBucket rq) (roRestoreRequest rq) rq
    response = receiveEmpty RestoreObjectResponse

data RestoreObjectResponse = RestoreObjectResponse
   deriving (Eq, Show)
