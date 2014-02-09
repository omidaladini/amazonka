{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketAcl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets the access control policy for the bucket.
module Network.AWS.S3.GetBucketAcl where

import qualified Data.Text        as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
getBucketAcl :: Text
             -> GetBucketAcl
getBucketAcl p1 = GetBucketAcl
    { gbaBucket = p1
    }

data GetBucketAcl = GetBucketAcl
    { gbaBucket :: !Text
    } deriving (Generic)

instance ToHeaders GetBucketAcl

instance ToPath GetBucketAcl where
    toPath GetBucketAcl{..} = Text.concat
        [ "/"
        , toText gbaBucket
        ]

instance ToQuery GetBucketAcl where
    toQuery GetBucketAcl{..} = queryFromList
        [ "acl"
        ]

instance AWSRequest GetBucketAcl where
    type Er GetBucketAcl = S3Error
    type Rs GetBucketAcl = GetBucketAclResponse
    request  = getS3 service
    response = undefined

data GetBucketAclResponse = GetBucketAclResponse
    { gbarGrants :: [Grant]
      -- ^ A list of grants.
    , gbarOwner :: Maybe Owner
    } deriving (Eq, Show, Generic)

instance FromXML GetBucketAclResponse where
    fromXMLOptions = xmlOptions
