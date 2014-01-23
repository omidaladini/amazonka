{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.GetPasswordData
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves the encrypted administrator password for the instances running
-- Windows. The Windows password is only generated the first time an AMI is
-- launched. It is not generated for rebundled AMIs or after the password is
-- changed on an instance. The password is encrypted using the key pair that
-- you provided.
module Network.AWS.EC2.GetPasswordData where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data GetPasswordData = GetPasswordData
    { gpdrDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , gpdrInstanceId :: !Text
      -- ^ The ID of the instance for which you want the Windows administrator
      -- password.
    } deriving (Eq, Show, Generic)

instance ToQuery GetPasswordData

instance AWSRequest GetPasswordData where
    type Er GetPasswordData = EC2Error
    type Rs GetPasswordData = GetPasswordDataResponse
    request = v2Query service GET "GetPasswordData"

data GetPasswordDataResponse = GetPasswordDataResponse
    { gpdrrsInstanceId :: Maybe Text
      -- ^ The ID of the instance whose Windows administrator password was requested.
    , gpdrrsPasswordData :: Maybe Text
      -- ^ The Windows administrator password of the specified instance.
    , gpdrrsTimestamp :: Maybe UTCTime
      -- ^ The time the data was last updated.
    } deriving (Eq, Show, Generic)

instance FromXML GetPasswordDataResponse where
    fromXMLOptions = xmlOptions
