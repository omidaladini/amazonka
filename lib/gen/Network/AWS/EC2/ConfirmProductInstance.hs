{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ConfirmProductInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ConfirmProductInstance operation returns true if the specified product
-- code is attached to the specified instance. The operation returns false if
-- the product code is not attached to the instance. The
-- ConfirmProductInstance operation can only be executed by the owner of the
-- AMI. This feature is useful when an AMI owner is providing support and
-- wants to verify whether a user's instance is eligible.
module Network.AWS.EC2.ConfirmProductInstance where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
confirmProductInstance :: Text
                       -- ^ The ID of the instance to confirm.
                       -> Text
                       -- ^ The product code to confirm.
                       -> ConfirmProductInstance
confirmProductInstance p1 p2 = ConfirmProductInstance
    { cpirInstanceId = p1
    , cpirProductCode = p2
    , cpirDryRun = Nothing
    }

data ConfirmProductInstance = ConfirmProductInstance
    { cpirDryRun :: Maybe Bool
    , cpirInstanceId :: !Text
      -- ^ The ID of the instance to confirm.
    , cpirProductCode :: !Text
      -- ^ The product code to confirm.
    } deriving (Eq, Show, Generic)

instance ToQuery ConfirmProductInstance

instance AWSRequest ConfirmProductInstance where
    type Er ConfirmProductInstance = EC2Error
    type Rs ConfirmProductInstance = ConfirmProductInstanceResponse
    request = getQuery service "ConfirmProductInstance"

data ConfirmProductInstanceResponse = ConfirmProductInstanceResponse
    { cpirrOwnerId :: Maybe Text
      -- ^ The instance owner's account ID. Only present if the product code is
      -- attached to the instance.
    } deriving (Eq, Show, Generic)

instance FromXML ConfirmProductInstanceResponse where
    fromXMLOptions = xmlOptions
