{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.CloneStack
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a clone of a specified stack. For more information, see Clone a
-- Stack. Required Permissions: To use this action, an IAM user must have an
-- attached policy that explicitly grants permissions. For more information on
-- user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.CloneStack where

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
cloneStack :: Text
           -> Text
           -> CloneStack
cloneStack p1 p2 = undefined $ CloneStack
    { cssServiceRoleArn = p1
    , cssSourceStackId = p2
    , cssAttributes = Map.empty
    , cssCloneAppIds = []
    , cssClonePermissions = Nothing
    , cssConfigurationManager = Nothing
    , cssCustomCookbooksSource = Nothing
    , cssCustomJson = Nothing
    , cssDefaultAvailabilityZone = Nothing
    , cssDefaultInstanceProfileArn = Nothing
    , cssDefaultOs = Nothing
    , cssDefaultRootDeviceType = Nothing
    , cssDefaultSshKeyName = Nothing
    , cssDefaultSubnetId = Nothing
    , cssHostnameTheme = Nothing
    , cssName = Nothing
    , cssRegion = Nothing
    , cssUseCustomCookbooks = Nothing
    , cssVpcId = Nothing
    }

data CloneStack = CloneStack
    { cssAttributes :: HashMap StackAttributesKeys Text
      -- ^ A list of stack attributes and values as key/value pairs to be added to the
      -- cloned stack.
    , cssCloneAppIds :: [Text]
      -- ^ A list of source stack app IDs to be included in the cloned stack.
    , cssClonePermissions :: Maybe Bool
      -- ^ Whether to clone the source stack's permissions.
    , cssConfigurationManager :: Maybe StackConfigurationManager
      -- ^ The configuration manager. When you clone a stack we recommend that you use
      -- the configuration manager to specify the Chef version, 0.9 or 11.4. The
      -- default value is currently 0.9. However, we expect to change the default
      -- value to 11.4 in September 2013.
    , cssCustomCookbooksSource :: Maybe Source
      -- ^ Contains the information required to retrieve an app or cookbook from a
      -- repository. For more information, see Creating Apps or Custom Recipes and
      -- Cookbooks.
    , cssCustomJson :: Maybe Text
      -- ^ A string that contains user-defined, custom JSON. It is used to override
      -- the corresponding default stack configuration JSON values. The string
      -- should be in the following format and must escape characters such as '"'.:
      -- "{\"key1\": \"value1\", \"key2\": \"value2\",...}" For more information on
      -- custom JSON, see Use Custom JSON to Modify the Stack Configuration JSON.
    , cssDefaultAvailabilityZone :: Maybe Text
      -- ^ The cloned stack's default Availability Zone, which must be in the
      -- specified region. For more information, see Regions and Endpoints. If you
      -- also specify a value for DefaultSubnetId, the subnet must be in the same
      -- zone. For more information, see the VpcId parameter description.
    , cssDefaultInstanceProfileArn :: Maybe Text
      -- ^ The ARN of an IAM profile that is the default profile for all of the
      -- stack's EC2 instances. For more information about IAM ARNs, see Using
      -- Identifiers.
    , cssDefaultOs :: Maybe Text
      -- ^ The cloned stack's default operating system, which must be set to Amazon
      -- Linux or Ubuntu 12.04 LTS. The default option is Amazon Linux.
    , cssDefaultRootDeviceType :: Maybe RootDeviceType
      -- ^ The default root device type. This value is used by default for all
      -- instances in the cloned stack, but you can override it when you create an
      -- instance. For more information, see Storage for the Root Device.
    , cssDefaultSshKeyName :: Maybe Text
      -- ^ A default SSH key for the stack instances. You can override this value when
      -- you create or update an instance.
    , cssDefaultSubnetId :: Maybe Text
      -- ^ The stack's default subnet ID. All instances will be launched into this
      -- subnet unless you specify otherwise when you create the instance. If you
      -- also specify a value for DefaultAvailabilityZone, the subnet must be in the
      -- same zone. For information on default values and when this parameter is
      -- required, see the VpcId parameter description.
    , cssHostnameTheme :: Maybe Text
      -- ^ The stack's host name theme, with spaces are replaced by underscores. The
      -- theme is used to generate host names for the stack's instances. By default,
      -- HostnameTheme is set to Layer_Dependent, which creates host names by
      -- appending integers to the layer's short name. The other themes are:
      -- Baked_Goods Clouds European_Cities Fruits Greek_Deities
      -- Legendary_Creatures_from_Japan Planets_and_Moons Roman_Deities
      -- Scottish_Islands US_Cities Wild_Cats To obtain a generated host name, call
      -- GetHostNameSuggestion, which returns a host name based on the current
      -- theme.
    , cssName :: Maybe Text
      -- ^ The cloned stack name.
    , cssRegion :: Maybe Text
      -- ^ The cloned stack AWS region, such as "us-east-1". For more information
      -- about AWS regions, see Regions and Endpoints.
    , cssServiceRoleArn :: !Text
      -- ^ The stack AWS Identity and Access Management (IAM) role, which allows AWS
      -- OpsWorks to work with AWS resources on your behalf. You must set this
      -- parameter to the Amazon Resource Name (ARN) for an existing IAM role. If
      -- you create a stack by using the AWS OpsWorks console, it creates the role
      -- for you. You can obtain an existing stack's IAM ARN programmatically by
      -- calling DescribePermissions. For more information about IAM ARNs, see Using
      -- Identifiers. You must set this parameter to a valid service role ARN or the
      -- action will fail; there is no default value. You can specify the source
      -- stack's service role ARN, if you prefer, but you must do so explicitly.
    , cssSourceStackId :: !Text
      -- ^ The source stack ID.
    , cssUseCustomCookbooks :: Maybe Bool
      -- ^ Whether to use custom cookbooks.
    , cssVpcId :: Maybe Text
      -- ^ The ID of the VPC that the cloned stack is to be launched into. It must be
      -- in the specified region. All instances will be launched into this VPC, and
      -- you cannot change the ID later. If your account supports EC2 Classic, the
      -- default value is no VPC. If your account does not support EC2 Classic, the
      -- default value is the default VPC for the specified region. If the VPC ID
      -- corresponds to a default VPC and you have specified either the
      -- DefaultAvailabilityZone or the DefaultSubnetId parameter only, AWS OpsWorks
      -- infers the value of the other parameter. If you specify neither parameter,
      -- AWS OpsWorks sets these parameters to the first valid Availability Zone for
      -- the specified region and the corresponding default VPC subnet ID,
      -- respectively. If you specify a nondefault VPC ID, note the following: It
      -- must belong to a VPC in your account that is in the specified region. You
      -- must specify a value for DefaultSubnetId. For more information on how to
      -- use AWS OpsWorks with a VPC, see Running a Stack in a VPC. For more
      -- information on default VPC and EC2 Classic, see Supported Platforms.
    } deriving (Eq, Show, Generic)

instance ToJSON CloneStack

instance AWSRequest CloneStack where
    type Er CloneStack = OpsWorksError
    type Rs CloneStack = CloneStackResponse
    request  = getJSON service
    response = responseJSON

data CloneStackResponse = CloneStackResponse
    { cssrsStackId :: Maybe Text
      -- ^ The cloned stack ID.
    } deriving (Eq, Show, Generic)

instance FromJSON CloneStackResponse
