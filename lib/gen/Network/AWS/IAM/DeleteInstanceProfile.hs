{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.DeleteInstanceProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified instance profile. The instance profile must not have
-- an associated role. Make sure you do not have any Amazon EC2 instances
-- running with the instance profile you are about to delete. Deleting a role
-- or instance profile that is associated with a running instance will break
-- any applications running on the instance. For more information about
-- instance profiles, go to About Instance Profiles.
-- https://iam.amazonaws.com/ ?Action=DeleteInstanceProfile
-- &InstanceProfileName=Webserver &Version=2010-05-08 &AUTHPARAMS
-- 90c18667-99f3-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.DeleteInstanceProfile where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

data DeleteInstanceProfile = DeleteInstanceProfile
    { dipInstanceProfileName :: !Text
      -- ^ Name of the instance profile to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteInstanceProfile

instance AWSRequest DeleteInstanceProfile where
    type Er DeleteInstanceProfile = IAMError
    type Rs DeleteInstanceProfile = DeleteInstanceProfileResponse
    request  = postQuery service "DeleteInstanceProfile"
    response = responseXML

data DeleteInstanceProfileResponse = DeleteInstanceProfileResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteInstanceProfileResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteInstanceProfileResponse"
