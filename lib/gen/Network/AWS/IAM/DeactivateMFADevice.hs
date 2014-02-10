{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.DeactivateMFADevice
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deactivates the specified MFA device and removes it from association with
-- the user name for which it was originally enabled.
-- https://iam.amazonaws.com/ ?Action=DeactivateMFADevice &UserName=Bob
-- &SerialNumber=R1234 &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.DeactivateMFADevice where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

data DeactivateMFADevice = DeactivateMFADevice
    { dmfadSerialNumber :: !Text
      -- ^ The serial number that uniquely identifies the MFA device. For virtual MFA
      -- devices, the serial number is the device ARN.
    , dmfadUserName :: !Text
      -- ^ Name of the user whose MFA device you want to deactivate.
    } deriving (Eq, Show, Generic)

instance ToQuery DeactivateMFADevice

instance AWSRequest DeactivateMFADevice where
    type Er DeactivateMFADevice = IAMError
    type Rs DeactivateMFADevice = DeactivateMFADeviceResponse
    request  = postQuery service "DeactivateMFADevice"
    response = responseXML

data DeactivateMFADeviceResponse = DeactivateMFADeviceResponse
    deriving (Eq, Show, Generic)

instance FromXML DeactivateMFADeviceResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeactivateMFADeviceResponse"
