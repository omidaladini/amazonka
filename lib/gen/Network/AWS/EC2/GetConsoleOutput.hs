{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.GetConsoleOutput
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The GetConsoleOutput operation retrieves console output for the specified
-- instance. Instance console output is buffered and posted shortly after
-- instance boot, reboot, and termination. Amazon EC2 preserves the most
-- recent 64 KB output which will be available for at least one hour after the
-- most recent post.
module Network.AWS.EC2.GetConsoleOutput where

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
getConsoleOutput :: Text
                 -> GetConsoleOutput
getConsoleOutput p1 = undefined $ GetConsoleOutput
    { gcorInstanceId = p1
    , gcorDryRun = Nothing
    }

data GetConsoleOutput = GetConsoleOutput
    { gcorDryRun :: Maybe Bool
    , gcorInstanceId :: !Text
      -- ^ The ID of the instance for which you want console output.
    } deriving (Eq, Show, Generic)

instance ToQuery GetConsoleOutput

instance AWSRequest GetConsoleOutput where
    type Er GetConsoleOutput = EC2Error
    type Rs GetConsoleOutput = GetConsoleOutputResponse
    request = getQuery service "GetConsoleOutput"

data GetConsoleOutputResponse = GetConsoleOutputResponse
    { gcorrsInstanceId :: Maybe Text
      -- ^ The ID of the instance whose console output was requested.
    , gcorrsOutput :: Maybe Text
      -- ^ The console output, Base64 encoded.
    , gcorrsTimestamp :: Maybe UTCTime
      -- ^ The time the output was last updated.
    } deriving (Eq, Show, Generic)

instance FromXML GetConsoleOutputResponse where
    fromXMLOptions = xmlOptions
