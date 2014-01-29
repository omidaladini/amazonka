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

-- | Convenience method utilising default fields where applicable.
getObjectAcl :: Text -- ^ Bucket
             -> Text -- ^ Key
             -> AWS (Either S3Error GetObjectAclResponse)
getObjectAcl p1 p2 = undefined $ GetObjectAcl
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

instance ToHeaders GetObjectAcl

instance ToPath GetObjectAcl where
    toPath GetObjectAcl{..} = Text.concat
        [ "/"
        , toText goaBucket
        , "/"
        , toText goaKey
        ]

instance ToQuery GetObjectAcl where
    toQuery GetObjectAcl{..} = List
        [ "acl&versionId" =? goaVersionId
        ]

instance AWSRequest GetObjectAcl where
    type Er GetObjectAcl = S3Error
    type Rs GetObjectAcl = GetObjectAclResponse
    request  = getS3 service
    response = undefined

data GetObjectAclResponse = GetObjectAclResponse
    { goarsGrants :: [Grant]
      -- ^ A list of grants.
    , goarsOwner :: Maybe Owner
    } deriving (Eq, Show, Generic)

instance FromXML GetObjectAclResponse where
    fromXMLOptions = xmlOptions
