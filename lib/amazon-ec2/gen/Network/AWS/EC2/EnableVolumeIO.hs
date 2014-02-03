{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.EnableVolumeIO
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enable IO on the volume after an event has occured.
module Network.AWS.EC2.EnableVolumeIO where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
enableVolumeIO :: Text
               -> EnableVolumeIO
enableVolumeIO p1 = EnableVolumeIO
    { eviorVolumeId = p1
    , eviorDryRun = Nothing
    }

data EnableVolumeIO = EnableVolumeIO
    { eviorDryRun :: Maybe Bool
    , eviorVolumeId :: !Text
    } deriving (Eq, Show, Generic)

instance ToQuery EnableVolumeIO

instance AWSRequest EnableVolumeIO where
    type Er EnableVolumeIO = EC2Error
    type Rs EnableVolumeIO = EnableVolumeIOResponse
    request = getQuery service "EnableVolumeIO"

data EnableVolumeIOResponse = EnableVolumeIOResponse
    deriving (Eq, Show, Generic)

instance FromXML EnableVolumeIOResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot EnableVolumeIOResponse
