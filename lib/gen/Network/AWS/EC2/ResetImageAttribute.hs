{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ResetImageAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ResetImageAttribute operation resets an attribute of an AMI to its
-- default value. The productCodes attribute cannot be reset.
module Network.AWS.EC2.ResetImageAttribute where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
resetImageAttribute :: ResetImageAttributeName
                    -- ^ The name of the attribute being reset. Available attribute names:
                    -- launchPermission.
                    -> Text
                    -- ^ The ID of the AMI whose attribute is being reset.
                    -> ResetImageAttribute
resetImageAttribute p1 p2 = ResetImageAttribute
    { riadAttribute = p1
    , riadImageId = p2
    , riadDryRun = Nothing
    }

data ResetImageAttribute = ResetImageAttribute
    { riadAttribute :: !ResetImageAttributeName
      -- ^ The name of the attribute being reset. Available attribute names:
      -- launchPermission.
    , riadDryRun :: Maybe Bool
    , riadImageId :: !Text
      -- ^ The ID of the AMI whose attribute is being reset.
    } deriving (Eq, Show, Generic)

instance ToQuery ResetImageAttribute

instance AWSRequest ResetImageAttribute where
    type Er ResetImageAttribute = EC2Error
    type Rs ResetImageAttribute = ResetImageAttributeResponse
    request  = postQuery service "ResetImageAttribute"
    response = responseXML

data ResetImageAttributeResponse = ResetImageAttributeResponse
    deriving (Eq, Show, Generic)

instance FromXML ResetImageAttributeResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ResetImageAttributeResponse"
