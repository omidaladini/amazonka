{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.ModifyOptionGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies an existing option group. https://rds.amazonaws.com/
-- ?Action=ModifyOptionGroup &OptionGroupName=myoptiongroup
-- &OptionsToInclude=OEM &DBSecurityGroupMemberships=default
-- &ApplyImmediately=true myoptiongroup Test option group oracle-se1 11.2 OEM
-- Oracle Enterprise Manager 1158 default ACTIVE
-- ed662948-a57b-11df-9e38-7ffab86c801f https://rds.amazonaws.com/
-- ?Action=ModifyOptionGroup &OptionGroupName=myoptiongroup
-- &OptionsToRemove=OEM &ApplyImmediately=true myoptiongroup Test option group
-- oracle-se1 11.2 ed662948-a57b-11df-9e38-7ffab86c801f.
module Network.AWS.RDS.ModifyOptionGroup where

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

import Network.AWS.RDS.Service
import Network.AWS.RDS.Types

data ModifyOptionGroup = ModifyOptionGroup
    { mogmApplyImmediately :: Maybe Bool
      -- ^ Indicates whether the changes should be applied immediately, or during the
      -- next maintenance window for each instance associated with the option group.
    , mogmOptionGroupName :: !Text
      -- ^ The name of the option group to be modified. cannot be removed from an
      -- option group while DB instances are associated with the option group. -->
      -- Permanent options, such as the TDE option for Oracle Advanced Security TDE,
      -- cannot be removed from an option group, and that option group cannot be
      -- removed from a DB instance once it is associated with a DB instance.
    , mogmOptionsToInclude :: [OptionConfiguration]
      -- ^ Options in this list are added to the option group or, if already present,
      -- the specified configuration is used to update the existing configuration.
    , mogmOptionsToRemove :: [Text]
      -- ^ Options in this list are removed from the option group.
    } deriving (Eq, Show, Generic)

instance ToQuery ModifyOptionGroup

instance AWSRequest ModifyOptionGroup where
    type Er ModifyOptionGroup = RDSError
    type Rs ModifyOptionGroup = ModifyOptionGroupResponse
    request = getQuery service "ModifyOptionGroup"

data ModifyOptionGroupResponse = ModifyOptionGroupResponse
    { mogmrsOptionGroup :: Maybe OptionGroup
    } deriving (Eq, Show, Generic)

instance FromXML ModifyOptionGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ModifyOptionGroupResponse"
        :| ["ModifyOptionGroupResult"]
