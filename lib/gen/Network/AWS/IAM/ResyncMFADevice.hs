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

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

data ResyncMFADevice = ResyncMFADevice
    { rmfadAuthenticationCode1 :: !Text
      -- ^ An authentication code emitted by the device.
    , rmfadAuthenticationCode2 :: !Text
      -- ^ A subsequent authentication code emitted by the device.
    , rmfadSerialNumber :: !Text
      -- ^ Serial number that uniquely identifies the MFA device.
    , rmfadUserName :: !Text
      -- ^ Name of the user whose MFA device you want to resynchronize.
    } deriving (Eq, Show, Generic)

instance ToQuery ResyncMFADevice

instance AWSRequest ResyncMFADevice where
    type Er ResyncMFADevice = IAMError
    type Rs ResyncMFADevice = ResyncMFADeviceResponse
    request  = postQuery service "ResyncMFADevice"
    response = responseXML

data ResyncMFADeviceResponse = ResyncMFADeviceResponse
    deriving (Eq, Show, Generic)

instance FromXML ResyncMFADeviceResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ResyncMFADeviceResponse"
