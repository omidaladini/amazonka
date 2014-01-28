{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.CreateVirtualMFADevice
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new virtual MFA device for the AWS account. After creating the
-- virtual MFA, use EnableMFADevice to attach the MFA device to an IAM user.
-- For more information about creating and working with virtual MFA devices,
-- go to Using a Virtual MFA Device in Using AWS Identity and Access
-- Management. For information about limits on the number of MFA devices you
-- can create, see Limitations on Entities in Using AWS Identity and Access
-- Management. The seed information contained in the QR code and the Base32
-- string should be treated like any other secret access information, such as
-- your AWS access keys or your passwords. After you provision your virtual
-- device, you should ensure that the information is destroyed following
-- secure procedures. https://iam.amazonaws.com/
-- ?Action=CreateVirtualMFADevice &VirtualMFADeviceName=ExampleName &Path=/
-- &Version=2010-05-08 &AUTHPARAMS arn:aws:iam::123456789012:mfa/ExampleName
-- 2K5K5XTLA7GGE75TQLYEXAMPLEEXAMPLEEXAMPLECHDFW4KJYZ6 UFQ75LL7COCYKM
-- 89504E470D0A1A0AASDFAHSDFKJKLJFKALSDFJASDF
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.CreateVirtualMFADevice where

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

data CreateVirtualMFADevice = CreateVirtualMFADevice
    { cvmfadrPath :: Maybe Text
      -- ^ The path for the virtual MFA device. For more information about paths, see
      -- Identifiers for IAM Entities in Using AWS Identity and Access Management.
      -- This parameter is optional. If it is not included, it defaults to a slash
      -- (/).
    , cvmfadrVirtualMFADeviceName :: !Text
      -- ^ The name of the virtual MFA device. Use with path to uniquely identify a
      -- virtual MFA device.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateVirtualMFADevice

instance AWSRequest CreateVirtualMFADevice where
    type Er CreateVirtualMFADevice = IAMError
    type Rs CreateVirtualMFADevice = CreateVirtualMFADeviceResponse
    request = getQuery service "CreateVirtualMFADevice"

data CreateVirtualMFADeviceResponse = CreateVirtualMFADeviceResponse
    { cvmfadrrsVirtualMFADevice :: VirtualMFADevice
      -- ^ A newly created virtual MFA device.
    } deriving (Eq, Show, Generic)

instance FromXML CreateVirtualMFADeviceResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateVirtualMFADeviceResponse"
        :| ["CreateVirtualMFADeviceResult"]