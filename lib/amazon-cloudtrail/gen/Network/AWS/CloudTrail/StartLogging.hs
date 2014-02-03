{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudTrail.StartLogging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Starts the recording of AWS API calls and log file delivery for a trail.
module Network.AWS.CloudTrail.StartLogging where

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

import Network.AWS.CloudTrail.Service
import Network.AWS.CloudTrail.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
startLogging :: Text
             -> StartLogging
startLogging p1 = StartLogging
    { slsName = p1
    }

data StartLogging = StartLogging
    { slsName :: !Text
      -- ^ The name of the trail for which CloudTrail logs AWS API calls.
    } deriving (Eq, Show, Generic)

instance ToJSON StartLogging where
    toJSON = genericToJSON jsonOptions

instance AWSRequest StartLogging where
    type Er StartLogging = CloudTrailError
    type Rs StartLogging = StartLoggingResponse
    request  = getJSON service
    response = responseJSON

data StartLoggingResponse = StartLoggingResponse
    deriving (Eq, Show, Generic)

instance FromJSON StartLoggingResponse where
    fromJSON = genericFromJSON jsonOptions

