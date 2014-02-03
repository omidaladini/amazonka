{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.GetInstanceProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves information about the specified instance profile, including the
-- instance profile's path, GUID, ARN, and role. For more information about
-- instance profiles, go to About Instance Profiles. For more information
-- about ARNs, go to ARNs. https://iam.amazonaws.com/
-- ?Action=GetInstanceProfile &InstanceProfileName=Webserver
-- &Version=2010-05-08 &AUTHPARAMS AIPAD5ARO2C5EXAMPLE3G
-- /application_abc/component_xyz/
-- arn:aws:iam::123456789012:role/application_abc/component_xyz/S3Access
-- S3Access
-- {"Version":"2012-10-17","Statement":[{"Effect":"Allow","Principal":{"Service":["ec2.amazonaws.com"]},"Action":["sts:AssumeRole"]}]}
-- 2012-05-09T15:45:35Z AROACVYKSVTSZFEXAMPLE Webserver
-- /application_abc/component_xyz/
-- arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Webserver
-- 2012-05-09T16:11:10Z 37289fda-99f2-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.GetInstanceProfile where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
getInstanceProfile :: Text
                   -> GetInstanceProfile
getInstanceProfile p1 = GetInstanceProfile
    { giprInstanceProfileName = p1
    }

data GetInstanceProfile = GetInstanceProfile
    { giprInstanceProfileName :: !Text
      -- ^ Name of the instance profile to get information about.
    } deriving (Eq, Show, Generic)

instance ToQuery GetInstanceProfile

instance AWSRequest GetInstanceProfile where
    type Er GetInstanceProfile = IAMError
    type Rs GetInstanceProfile = GetInstanceProfileResponse
    request = getQuery service "GetInstanceProfile"

data GetInstanceProfileResponse = GetInstanceProfileResponse
    { giprrsInstanceProfile :: InstanceProfile
      -- ^ Information about the instance profile.
    } deriving (Eq, Show, Generic)

instance FromXML GetInstanceProfileResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetInstanceProfileResponse"
        :| ["GetInstanceProfileResult"]
