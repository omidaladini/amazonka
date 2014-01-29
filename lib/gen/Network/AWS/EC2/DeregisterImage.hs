{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeregisterImage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeregisterImage operation deregisters an AMI. Once deregistered,
-- instances of the AMI can no longer be launched.
module Network.AWS.EC2.DeregisterImage where

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
deregisterImage :: Text
                -> DeregisterImage
deregisterImage p1 = undefined $ DeregisterImage
    { dirImageId = p1
    , dirDryRun = Nothing
    }

data DeregisterImage = DeregisterImage
    { dirDryRun :: Maybe Bool
    , dirImageId :: !Text
      -- ^ The ID of the AMI to deregister.
    } deriving (Eq, Show, Generic)

instance ToQuery DeregisterImage

instance AWSRequest DeregisterImage where
    type Er DeregisterImage = EC2Error
    type Rs DeregisterImage = DeregisterImageResponse
    request = getQuery service "DeregisterImage"

data DeregisterImageResponse = DeregisterImageResponse
    deriving (Eq, Show, Generic)

instance FromXML DeregisterImageResponse where
    fromXMLOptions = xmlOptions
