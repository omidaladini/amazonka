{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.DeleteVirtualMFADevice
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a virtual MFA device. You must deactivate a user's virtual MFA
-- device before you can delete it. For information about deactivating MFA
-- devices, see DeactivateMFADevice. https://iam.amazonaws.com/
-- ?Action=DeleteVirtualMFADevice
-- &SerialNumber=arn:aws:iam::123456789012:mfa/ExampleName &Version=2010-05-08
-- &AUTHPARAMS arn:aws:iam::123456789012:mfa/ExampleName
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.DeleteVirtualMFADevice where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteVirtualMFADevice :: Text
                       -> DeleteVirtualMFADevice
deleteVirtualMFADevice p1 = DeleteVirtualMFADevice
    { dvmfadrSerialNumber = p1
    }

data DeleteVirtualMFADevice = DeleteVirtualMFADevice
    { dvmfadrSerialNumber :: !Text
      -- ^ The serial number that uniquely identifies the MFA device. For virtual MFA
      -- devices, the serial number is the same as the ARN.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteVirtualMFADevice

instance AWSRequest DeleteVirtualMFADevice where
    type Er DeleteVirtualMFADevice = IAMError
    type Rs DeleteVirtualMFADevice = DeleteVirtualMFADeviceResponse
    request = getQuery service "DeleteVirtualMFADevice"

data DeleteVirtualMFADeviceResponse = DeleteVirtualMFADeviceResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteVirtualMFADeviceResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteVirtualMFADeviceResponse"
