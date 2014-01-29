{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.DescribeTapeArchives
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for DescribeTapeArchives
module Network.AWS.StorageGateway.DescribeTapeArchives where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.StorageGateway.Service
import Network.AWS.StorageGateway.Types

-- | Convenience method utilising default fields where applicable.
describeTapeArchives :: AWS (Either StorageGatewayError DescribeTapeArchivesResponse)
describeTapeArchives = undefined $ DescribeTapeArchives
    { dtaiLimit = Nothing
    , dtaiMarker = Nothing
    , dtaiTapeARNs = []
    }

data DescribeTapeArchives = DescribeTapeArchives
    { dtaiLimit :: Maybe Int
    , dtaiMarker :: Maybe Text
    , dtaiTapeARNs :: [Text]
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeTapeArchives

instance AWSRequest DescribeTapeArchives where
    type Er DescribeTapeArchives = StorageGatewayError
    type Rs DescribeTapeArchives = DescribeTapeArchivesResponse
    request  = getJSON service
    response = responseJSON

data DescribeTapeArchivesResponse = DescribeTapeArchivesResponse
    { dtairsMarker :: Maybe Text
    , dtairsTapeArchives :: [TapeArchive]
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeTapeArchivesResponse
