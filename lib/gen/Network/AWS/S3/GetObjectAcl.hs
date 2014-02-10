{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetObjectAcl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the access control list (ACL) of an object.
module Network.AWS.S3.GetObjectAcl where

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
getObjectAcl :: Text
             -> Text
             -> GetObjectAcl
getObjectAcl p1 p2 = GetObjectAcl
    { goaBucket = p1
    , goaKey = p2
    , goaVersionId = Nothing
    }

data GetObjectAcl = GetObjectAcl
    { goaBucket :: !Text
    , goaKey :: !Text
    , goaVersionId :: Maybe Text
      -- ^ VersionId used to reference a specific version of the object.
    } deriving (Generic)

instance ToPath GetObjectAcl where
    toPath GetObjectAcl{..} = Text.concat
        [ "/"
        , toText goaBucket
        , "/"
        , toText goaKey
        ]

instance ToQuery GetObjectAcl where
    toQuery GetObjectAcl{..} = queryFromList
        [ "acl&versionId" =? goaVersionId
        ]

instance ToHeaders GetObjectAcl

instance AWSRequest GetObjectAcl where
    type Er GetObjectAcl = S3Error
    type Rs GetObjectAcl = GetObjectAclResponse
    request rq = s3 GET (service $ goaBucket rq) rq
    response = receiveXML $ \hs doc -> GetObjectAclResponse
        <$> xml "Grants" doc
        <*> xml "Owner" doc

data GetObjectAclResponse = GetObjectAclResponse
    { goarAccessControlList :: [Grant]
      -- ^ A list of grants.
    , goarOwner :: Maybe Owner
    } deriving (Eq, Show)
