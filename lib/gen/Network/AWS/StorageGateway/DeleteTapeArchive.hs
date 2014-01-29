{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.DeleteTapeArchive
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for DeleteTapeArchive
module Network.AWS.StorageGateway.DeleteTapeArchive where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteTapeArchive :: Text
                  -> DeleteTapeArchive
deleteTapeArchive p1 = undefined $ DeleteTapeArchive
    { dtajTapeARN = p1
    }

data DeleteTapeArchive = DeleteTapeArchive
    { dtajTapeARN :: !Text
    } deriving (Eq, Show, Generic)

instance ToJSON DeleteTapeArchive

instance AWSRequest DeleteTapeArchive where
    type Er DeleteTapeArchive = StorageGatewayError
    type Rs DeleteTapeArchive = DeleteTapeArchiveResponse
    request  = getJSON service
    response = responseJSON

data DeleteTapeArchiveResponse = DeleteTapeArchiveResponse
    { dtajrsTapeARN :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromJSON DeleteTapeArchiveResponse
