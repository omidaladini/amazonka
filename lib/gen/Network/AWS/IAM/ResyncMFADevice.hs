{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ResyncMFADevice
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Synchronizes the specified MFA device with AWS servers.
-- https://iam.amazonaws.com/ ?Action=ResyncMFADevice &UserName=Bob
-- &SerialNumber=R1234 &AuthenticationCode1=234567 &AuthenticationCode2=987654
-- &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.ResyncMFADevice where

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

import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields where applicable.
resyncMFADevice :: Text
                -> Text
                -> Text
                -> Text
                -> AWS (Either IAMError ResyncMFADeviceResponse)
resyncMFADevice p1 p2 p3 p4 = undefined $ ResyncMFADevice
    { rmfadrAuthenticationCode1 = p1
    , rmfadrAuthenticationCode2 = p2
    , rmfadrSerialNumber = p3
    , rmfadrUserName = p4
    }

data ResyncMFADevice = ResyncMFADevice
    { rmfadrAuthenticationCode1 :: !Text
      -- ^ An authentication code emitted by the device.
    , rmfadrAuthenticationCode2 :: !Text
      -- ^ A subsequent authentication code emitted by the device.
    , rmfadrSerialNumber :: !Text
      -- ^ Serial number that uniquely identifies the MFA device.
    , rmfadrUserName :: !Text
      -- ^ Name of the user whose MFA device you want to resynchronize.
    } deriving (Eq, Show, Generic)

instance ToQuery ResyncMFADevice

instance AWSRequest ResyncMFADevice where
    type Er ResyncMFADevice = IAMError
    type Rs ResyncMFADevice = ResyncMFADeviceResponse
    request = getQuery service "ResyncMFADevice"

data ResyncMFADeviceResponse = ResyncMFADeviceResponse
    deriving (Eq, Show, Generic)

instance FromXML ResyncMFADeviceResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ResyncMFADeviceResponse"
        :| ["ResyncMFADeviceResult"]
