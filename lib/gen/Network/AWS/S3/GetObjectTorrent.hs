{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetObjectTorrent
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Return torrent files from a bucket.
module Network.AWS.S3.GetObjectTorrent where

import           Control.Applicative
import qualified Data.Text           as Text
import           Network.AWS.Core
import           Network.AWS.S3.Service
import           Network.AWS.S3.Types

data GetObjectTorrent = GetObjectTorrent
    { gotBucket :: !Text
    , gotKey :: !Text
    } deriving (Generic)

instance ToPath GetObjectTorrent where
    toPath GetObjectTorrent{..} = Text.concat
        [ "/"
        , toText gotBucket
        , "/"
        , toText gotKey
        ]

instance ToQuery GetObjectTorrent where
    toQuery GetObjectTorrent{..} = queryFromList
        [ "torrent"
        ]

instance ToHeaders GetObjectTorrent

instance AWSRequest GetObjectTorrent where
    type Er GetObjectTorrent = S3Error
    type Rs GetObjectTorrent = GetObjectTorrentResponse
    request rq = s3 GET (service $ gotBucket rq) rq
    response = receiveBody $ \hs bdy -> GetObjectTorrentResponse
        <$> pure bdy

data GetObjectTorrentResponse = GetObjectTorrentResponse
    { gotrBody :: ResumableSource AWS ByteString
    } deriving (Show)
