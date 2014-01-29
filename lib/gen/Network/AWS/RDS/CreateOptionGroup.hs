{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.CreateOptionGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new option group. You can create up to 20 option groups.
-- https://rds.amazonaws.com/ ?Action=CreateOptionGroup
-- &OptionGroupName=myoptiongroup &EngineName=oracle-se1
-- &MajorEngineVersion=11.2 &OptionGroupDescription=Test option group 11.2
-- myoptiongroup oracle-se1 Test option group
-- b2416a8a-84c9-11e1-a264-0b23c28bc344.
module Network.AWS.RDS.CreateOptionGroup where

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

-- | Convenience method utilising default fields where applicable.
createOptionGroup :: Text
                  -> Text
                  -> Text
                  -> Text
                  -> AWS (Either RDSError CreateOptionGroupResponse)
createOptionGroup p1 p2 p3 p4 = undefined $ CreateOptionGroup
    { cogmEngineName = p1
    , cogmMajorEngineVersion = p2
    , cogmOptionGroupDescription = p3
    , cogmOptionGroupName = p4
    , cogmTags = []
    }

data CreateOptionGroup = CreateOptionGroup
    { cogmEngineName :: !Text
      -- ^ Specifies the name of the engine that this option group should be
      -- associated with.
    , cogmMajorEngineVersion :: !Text
      -- ^ Specifies the major version of the engine that this option group should be
      -- associated with.
    , cogmOptionGroupDescription :: !Text
      -- ^ The description of the option group.
    , cogmOptionGroupName :: !Text
      -- ^ Specifies the name of the option group to be created. Constraints: Must be
      -- 1 to 255 alphanumeric characters or hyphens First character must be a
      -- letter Cannot end with a hyphen or contain two consecutive hyphens Example:
      -- myoptiongroup.
    , cogmTags :: [Tag]
      -- ^ A list of tags.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateOptionGroup

instance AWSRequest CreateOptionGroup where
    type Er CreateOptionGroup = RDSError
    type Rs CreateOptionGroup = CreateOptionGroupResponse
    request = getQuery service "CreateOptionGroup"

data CreateOptionGroupResponse = CreateOptionGroupResponse
    { cogmrsOptionGroup :: Maybe OptionGroup
    } deriving (Eq, Show, Generic)

instance FromXML CreateOptionGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateOptionGroupResponse"
        :| ["CreateOptionGroupResult"]
