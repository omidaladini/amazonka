{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.ResetDBParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies the parameters of a DB parameter group to the engine/system
-- default value. To reset specific parameters submit a list of the following:
-- ParameterName and ApplyMethod. To reset the entire DB parameter group,
-- specify the DBParameterGroup name and ResetAllParameters parameters. When
-- resetting the entire group, dynamic parameters are updated immediately and
-- static parameters are set to pending-reboot to take effect on the next DB
-- instance restart or RebootDBInstance request. https://rds.amazonaws.com/
-- ?Action=ResetDBParameterGroup &DBParameterGroupName=mydbparametergroup
-- &Parameters.member.1.ParameterName=max_user_connections
-- &Parameters.member.1.ApplyMethod=pending-reboot
-- &Parameters.member.2.ParameterName=max_allowed_packet
-- &Parameters.member.2.ApplyMethod=immediate &ResetAllParameters=false
-- &Version=2013-05-15 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &AWSAccessKeyId= &Signature= mydbparametergroup
-- 071e758f-bf57-11de-9f9f-53d6aee22de9.
module Network.AWS.RDS.ResetDBParameterGroup where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
resetDBParameterGroup :: Text
                      -> ResetDBParameterGroup
resetDBParameterGroup p1 = undefined $ ResetDBParameterGroup
    { rdbpgmDBParameterGroupName = p1
    , rdbpgmParameters = []
    , rdbpgmResetAllParameters = Nothing
    }

data ResetDBParameterGroup = ResetDBParameterGroup
    { rdbpgmDBParameterGroupName :: !Text
      -- ^ The name of the DB parameter group. Constraints: Must be 1 to 255
      -- alphanumeric characters First character must be a letter Cannot end with a
      -- hyphen or contain two consecutive hyphens.
    , rdbpgmParameters :: [Parameter]
      -- ^ An array of parameter names, values, and the apply method for the parameter
      -- update. At least one parameter name, value, and apply method must be
      -- supplied; subsequent arguments are optional. A maximum of 20 parameters may
      -- be modified in a single request. MySQL Valid Values (for Apply method):
      -- immediate | pending-reboot You can use the immediate value with dynamic
      -- parameters only. You can use the pending-reboot value for both dynamic and
      -- static parameters, and changes are applied when DB instance reboots. Oracle
      -- Valid Values (for Apply method): pending-reboot.
    , rdbpgmResetAllParameters :: Maybe Bool
      -- ^ Specifies whether (true) or not (false) to reset all parameters in the DB
      -- parameter group to default values. Default: true.
    } deriving (Eq, Show, Generic)

instance ToQuery ResetDBParameterGroup

instance AWSRequest ResetDBParameterGroup where
    type Er ResetDBParameterGroup = RDSError
    type Rs ResetDBParameterGroup = ResetDBParameterGroupResponse
    request = getQuery service "ResetDBParameterGroup"

data ResetDBParameterGroupResponse = ResetDBParameterGroupResponse
    { rdbpgmrsDBParameterGroupName :: Maybe Text
      -- ^ The name of the DB parameter group.
    } deriving (Eq, Show, Generic)

instance FromXML ResetDBParameterGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ResetDBParameterGroupResponse"
        :| ["ResetDBParameterGroupResult"]
