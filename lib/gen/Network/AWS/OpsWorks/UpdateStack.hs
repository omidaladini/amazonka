{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.UpdateStack
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates a specified stack. Required Permissions: To use this action, an IAM
-- user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.UpdateStack where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.OpsWorks.Service
import Network.AWS.OpsWorks.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
updateStack :: Text
            -> UpdateStack
updateStack p1 = undefined $ UpdateStack
    { usrStackId = p1
    , usrAttributes = Map.empty
    , usrConfigurationManager = Nothing
    , usrCustomCookbooksSource = Nothing
    , usrCustomJson = Nothing
    , usrDefaultAvailabilityZone = Nothing
    , usrDefaultInstanceProfileArn = Nothing
    , usrDefaultOs = Nothing
    , usrDefaultRootDeviceType = Nothing
    , usrDefaultSshKeyName = Nothing
    , usrDefaultSubnetId = Nothing
    , usrHostnameTheme = Nothing
    , usrName = Nothing
    , usrServiceRoleArn = Nothing
    , usrUseCustomCookbooks = Nothing
    }

data UpdateStack = UpdateStack
    { usrAttributes :: HashMap StackAttributesKeys Text
      -- ^ One or more user-defined key/value pairs to be added to the stack
      -- attributes bag.
    , usrConfigurationManager :: Maybe StackConfigurationManager
      -- ^ The configuration manager. When you update a stack you can optionally use
      -- the configuration manager to specify the Chef version, 0.9 or 11.4. If you
      -- omit this parameter, AWS OpsWorks does not change the Chef version.
    , usrCustomCookbooksSource :: Maybe Source
      -- ^ Contains the information required to retrieve an app or cookbook from a
      -- repository. For more information, see Creating Apps or Custom Recipes and
      -- Cookbooks.
    , usrCustomJson :: Maybe Text
      -- ^ A string that contains user-defined, custom JSON. It is used to override
      -- the corresponding default stack configuration JSON values. The string
      -- should be in the following format and must escape characters such as '"'.:
      -- "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more information on
      -- custom JSON, see Use Custom JSON to Modify the Stack Configuration JSON.
    , usrDefaultAvailabilityZone :: Maybe Text
      -- ^ The stack's default Availability Zone, which must be in the specified
      -- region. For more information, see Regions and Endpoints. If you also
      -- specify a value for DefaultSubnetId, the subnet must be in the same zone.
      -- For more information, see CreateStack.
    , usrDefaultInstanceProfileArn :: Maybe Text
      -- ^ The ARN of an IAM profile that is the default profile for all of the
      -- stack's EC2 instances. For more information about IAM ARNs, see Using
      -- Identifiers.
    , usrDefaultOs :: Maybe Text
      -- ^ The stack's default operating system, which must be set to Amazon Linux or
      -- Ubuntu 12.04 LTS. The default option is Amazon Linux.
    , usrDefaultRootDeviceType :: Maybe RootDeviceType
      -- ^ The default root device type. This value is used by default for all
      -- instances in the cloned stack, but you can override it when you create an
      -- instance. For more information, see Storage for the Root Device.
    , usrDefaultSshKeyName :: Maybe Text
      -- ^ A default SSH key for the stack instances. You can override this value when
      -- you create or update an instance.
    , usrDefaultSubnetId :: Maybe Text
      -- ^ The stack's default subnet ID. All instances will be launched into this
      -- subnet unless you specify otherwise when you create the instance. If you
      -- also specify a value for DefaultAvailabilityZone, the subnet must be in
      -- that zone. For more information, see CreateStack.
    , usrHostnameTheme :: Maybe Text
      -- ^ The stack's new host name theme, with spaces are replaced by underscores.
      -- The theme is used to generate host names for the stack's instances. By
      -- default, HostnameTheme is set to Layer_Dependent, which creates host names
      -- by appending integers to the layer's short name. The other themes are:
      -- Baked_Goods Clouds European_Cities Fruits Greek_Deities
      -- Legendary_Creatures_from_Japan Planets_and_Moons Roman_Deities
      -- Scottish_Islands US_Cities Wild_Cats To obtain a generated host name, call
      -- GetHostNameSuggestion, which returns a host name based on the current
      -- theme.
    , usrName :: Maybe Text
      -- ^ The stack's new name.
    , usrServiceRoleArn :: Maybe Text
      -- ^ The stack AWS Identity and Access Management (IAM) role, which allows AWS
      -- OpsWorks to work with AWS resources on your behalf. You must set this
      -- parameter to the Amazon Resource Name (ARN) for an existing IAM role. For
      -- more information about IAM ARNs, see Using Identifiers. You must set this
      -- parameter to a valid service role ARN or the action will fail; there is no
      -- default value. You can specify the stack's current service role ARN, if you
      -- prefer, but you must do so explicitly.
    , usrStackId :: !Text
      -- ^ The stack ID.
    , usrUseCustomCookbooks :: Maybe Bool
      -- ^ Whether the stack uses custom cookbooks.
    } deriving (Eq, Show, Generic)

instance ToJSON UpdateStack

instance AWSRequest UpdateStack where
    type Er UpdateStack = OpsWorksError
    type Rs UpdateStack = UpdateStackResponse
    request  = getJSON service
    response = responseJSON

data UpdateStackResponse = UpdateStackResponse
    deriving (Eq, Show, Generic)

instance FromJSON UpdateStackResponse
