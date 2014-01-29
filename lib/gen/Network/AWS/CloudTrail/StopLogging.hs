{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudTrail.StopLogging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Suspends the recording of AWS API calls and log file delivery for the
-- specified trail. Under most circumstances, there is no need to use this
-- action. You can update a trail without stopping it first. This action is
-- the only way to stop recording.
module Network.AWS.CloudTrail.StopLogging where

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

-- | Convenience method utilising default fields where applicable.
stopLogging :: Text
            -> AWS (Either CloudTrailError StopLoggingResponse)
stopLogging p1 = undefined $ StopLogging
    { slrName = p1
    }

data StopLogging = StopLogging
    { slrName :: !Text
      -- ^ Communicates to CloudTrail the name of the trail for which to stop logging
      -- AWS API calls.
    } deriving (Eq, Show, Generic)

instance ToJSON StopLogging

instance AWSRequest StopLogging where
    type Er StopLogging = CloudTrailError
    type Rs StopLogging = StopLoggingResponse
    request  = getJSON service
    response = responseJSON

data StopLoggingResponse = StopLoggingResponse
    deriving (Eq, Show, Generic)

instance FromJSON StopLoggingResponse
