{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ModifyVpcAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for ModifyVpcAttribute
module Network.AWS.EC2.ModifyVpcAttribute where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
modifyVpcAttribute :: Text
                   -> ModifyVpcAttribute
modifyVpcAttribute p1 = ModifyVpcAttribute
    { mvadVpcId = p1
    , mvadEnableDnsHostnames = Nothing
    , mvadEnableDnsSupport = Nothing
    }

data ModifyVpcAttribute = ModifyVpcAttribute
    { mvadEnableDnsHostnames :: Maybe AttributeBooleanValue
      -- ^ Boolean value.
    , mvadEnableDnsSupport :: Maybe AttributeBooleanValue
      -- ^ Boolean value.
    , mvadVpcId :: !Text
    } deriving (Eq, Show, Generic)

instance ToQuery ModifyVpcAttribute

instance AWSRequest ModifyVpcAttribute where
    type Er ModifyVpcAttribute = EC2Error
    type Rs ModifyVpcAttribute = ModifyVpcAttributeResponse
    request  = postQuery service "ModifyVpcAttribute"
    response = responseXML

data ModifyVpcAttributeResponse = ModifyVpcAttributeResponse
    deriving (Eq, Show, Generic)

instance FromXML ModifyVpcAttributeResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ModifyVpcAttributeResponse"
