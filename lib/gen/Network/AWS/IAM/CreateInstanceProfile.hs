{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.CreateInstanceProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new instance profile. For information about instance profiles, go
-- to About Instance Profiles. For information about the number of instance
-- profiles you can create, see Limitations on IAM Entities in Using AWS
-- Identity and Access Management. https://iam.amazonaws.com/
-- ?Action=CreateInstanceProfile &InstanceProfileName=Webserver
-- &Path=/application_abc/component_xyz/ &Version=2010-05-08 &AUTHPARAMS
-- AIPAD5ARO2C5EXAMPLE3G Webserver /application_abc/component_xyz/
-- arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Webserver
-- 2012-05-09T16:11:10.222Z 974142ee-99f1-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.CreateInstanceProfile where

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
createInstanceProfile :: Text
                      -> AWS (Either IAMError CreateInstanceProfileResponse)
createInstanceProfile p1 = undefined $ CreateInstanceProfile
    { ciprInstanceProfileName = p1
    , ciprPath = Nothing
    }

data CreateInstanceProfile = CreateInstanceProfile
    { ciprInstanceProfileName :: !Text
      -- ^ Name of the instance profile to create.
    , ciprPath :: Maybe Text
      -- ^ The path to the instance profile. For more information about paths, see
      -- Identifiers for IAM Entities in Using AWS Identity and Access Management.
      -- This parameter is optional. If it is not included, it defaults to a slash
      -- (/).
    } deriving (Eq, Show, Generic)

instance ToQuery CreateInstanceProfile

instance AWSRequest CreateInstanceProfile where
    type Er CreateInstanceProfile = IAMError
    type Rs CreateInstanceProfile = CreateInstanceProfileResponse
    request = getQuery service "CreateInstanceProfile"

data CreateInstanceProfileResponse = CreateInstanceProfileResponse
    { ciprrsInstanceProfile :: InstanceProfile
      -- ^ Information about the instance profile.
    } deriving (Eq, Show, Generic)

instance FromXML CreateInstanceProfileResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateInstanceProfileResponse"
        :| ["CreateInstanceProfileResult"]
