{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.RetrieveTapeArchive
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.StorageGateway.V2013_06_30.RetrieveTapeArchive
    (
    -- * Request
      RetrieveTapeArchive
    -- ** Request constructor
    , retrieveTapeArchive
    -- ** Request lenses
    , rtaiGatewayARN
    , rtaiTapeARN

    -- * Response
    , RetrieveTapeArchiveResponse
    -- ** Response lenses
    , rtaoTapeARN
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'RetrieveTapeArchive' request.
retrieveTapeArchive :: Text -- ^ 'rtaiGatewayARN'
                    -> Text -- ^ 'rtaiTapeARN'
                    -> RetrieveTapeArchive
retrieveTapeArchive p1 p2 = RetrieveTapeArchive
    { _rtaiGatewayARN = p1
    , _rtaiTapeARN = p2
    }

data RetrieveTapeArchive = RetrieveTapeArchive
    { _rtaiGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _rtaiTapeARN :: Text
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
rtaiGatewayARN
    :: Functor f
    => (Text
    -> f (Text))
    -> RetrieveTapeArchive
    -> f RetrieveTapeArchive
rtaiGatewayARN f x =
    (\y -> x { _rtaiGatewayARN = y })
       <$> f (_rtaiGatewayARN x)
{-# INLINE rtaiGatewayARN #-}

rtaiTapeARN
    :: Functor f
    => (Text
    -> f (Text))
    -> RetrieveTapeArchive
    -> f RetrieveTapeArchive
rtaiTapeARN f x =
    (\y -> x { _rtaiTapeARN = y })
       <$> f (_rtaiTapeARN x)
{-# INLINE rtaiTapeARN #-}

instance ToPath RetrieveTapeArchive

instance ToQuery RetrieveTapeArchive

instance ToHeaders RetrieveTapeArchive

instance ToJSON RetrieveTapeArchive

data RetrieveTapeArchiveResponse = RetrieveTapeArchiveResponse
    { _rtaoTapeARN :: Maybe Text
    } deriving (Show, Generic)

rtaoTapeARN
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RetrieveTapeArchiveResponse
    -> f RetrieveTapeArchiveResponse
rtaoTapeARN f x =
    (\y -> x { _rtaoTapeARN = y })
       <$> f (_rtaoTapeARN x)
{-# INLINE rtaoTapeARN #-}

instance FromJSON RetrieveTapeArchiveResponse

instance AWSRequest RetrieveTapeArchive where
    type Sv RetrieveTapeArchive = StorageGateway
    type Rs RetrieveTapeArchive = RetrieveTapeArchiveResponse

    request = get
    response _ = jsonResponse
