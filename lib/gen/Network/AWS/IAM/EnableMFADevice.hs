{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.EnableMFADevice
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables the specified MFA device and associates it with the specified user
-- name. When enabled, the MFA device is required for every subsequent login
-- by the user name associated with the device. https://iam.amazonaws.com/
-- ?Action=EnableMFADevice &UserName=Bob &SerialNumber=R1234
-- &AuthenticationCode1=234567 &AuthenticationCode2=987654 &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.EnableMFADevice where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

data EnableMFADevice = EnableMFADevice
    { emfadAuthenticationCode1 :: !Text
      -- ^ An authentication code emitted by the device.
    , emfadAuthenticationCode2 :: !Text
      -- ^ A subsequent authentication code emitted by the device.
    , emfadSerialNumber :: !Text
      -- ^ The serial number that uniquely identifies the MFA device. For virtual MFA
      -- devices, the serial number is the device ARN.
    , emfadUserName :: !Text
      -- ^ Name of the user for whom you want to enable the MFA device.
    } deriving (Eq, Show, Generic)

instance ToQuery EnableMFADevice

instance AWSRequest EnableMFADevice where
    type Er EnableMFADevice = IAMError
    type Rs EnableMFADevice = EnableMFADeviceResponse
    request  = postQuery service "EnableMFADevice"
    response = responseXML

data EnableMFADeviceResponse = EnableMFADeviceResponse
    deriving (Eq, Show, Generic)

instance FromXML EnableMFADeviceResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnableMFADeviceResponse"
