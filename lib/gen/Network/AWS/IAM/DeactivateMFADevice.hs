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
deactivateMFADevice :: Text
                    -> Text
                    -> AWS (Either IAMError DeactivateMFADeviceResponse)
deactivateMFADevice p1 p2 = undefined $ DeactivateMFADevice
    { dmfadrSerialNumber = p1
    , dmfadrUserName = p2
    }

data DeactivateMFADevice = DeactivateMFADevice
    { dmfadrSerialNumber :: !Text
      -- ^ The serial number that uniquely identifies the MFA device. For virtual MFA
      -- devices, the serial number is the device ARN.
    , dmfadrUserName :: !Text
      -- ^ Name of the user whose MFA device you want to deactivate.
    } deriving (Eq, Show, Generic)

instance ToQuery DeactivateMFADevice

instance AWSRequest DeactivateMFADevice where
    type Er DeactivateMFADevice = IAMError
    type Rs DeactivateMFADevice = DeactivateMFADeviceResponse
    request = getQuery service "DeactivateMFADevice"

data DeactivateMFADeviceResponse = DeactivateMFADeviceResponse
    deriving (Eq, Show, Generic)

instance FromXML DeactivateMFADeviceResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeactivateMFADeviceResponse"
        :| ["DeactivateMFADeviceResult"]
