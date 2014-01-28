{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CopyImage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for CopyImage
module Network.AWS.EC2.CopyImage where

import           Control.Monad
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import           Text.XML.Generic

import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data CopyImage = CopyImage
    { cirClientToken :: Maybe Text
    , cirDescription :: Maybe Text
    , cirDryRun :: Maybe Bool
    , cirName :: Maybe Text
    , cirSourceImageId :: !Text
    , cirSourceRegion :: !Text
    } deriving (Eq, Show, Generic)

instance ToQuery CopyImage

instance AWSRequest CopyImage where
    type Er CopyImage = EC2Error
    type Rs CopyImage = CopyImageResponse
    request = getQuery service "CopyImage"

data CopyImageResponse = CopyImageResponse
    { cirrsImageId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromXML CopyImageResponse where
    fromXMLOptions = xmlOptions
