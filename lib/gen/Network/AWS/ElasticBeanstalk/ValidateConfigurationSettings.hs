{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Takes a set of configuration settings and either a configuration template
-- or environment, and determines whether those values are valid. This action
-- returns a list of messages indicating any errors or warnings associated
-- with the selection of option values.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &EnvironmentName=SampleAppVersion
-- &OptionSettings.member.1.Namespace=aws%3Aautoscaling%3Atrigger
-- &OptionSettings.member.1.OptionName=LowerThreshold
-- &OptionSettings.member.1.Value=1000000
-- &Operation=ValidateConfigurationSettings &AuthParams
-- 06f1cfff-f28f-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.ValidateConfigurationSettings where

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

import Network.AWS.ElasticBeanstalk.Service
import Network.AWS.ElasticBeanstalk.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
validateConfigurationSettings :: Text
                              -> [ConfigurationOptionSetting]
                              -> ValidateConfigurationSettings
validateConfigurationSettings p1 p2 = undefined $ ValidateConfigurationSettings
    { vcsmApplicationName = p1
    , vcsmOptionSettings = p2
    , vcsmEnvironmentName = Nothing
    , vcsmTemplateName = Nothing
    }

data ValidateConfigurationSettings = ValidateConfigurationSettings
    { vcsmApplicationName :: !Text
      -- ^ The name of the application that the configuration template or environment
      -- belongs to.
    , vcsmEnvironmentName :: Maybe Text
      -- ^ The name of the environment to validate the settings against. Condition:
      -- You cannot specify both this and a configuration template name.
    , vcsmOptionSettings :: [ConfigurationOptionSetting]
      -- ^ A list of the options and desired values to evaluate.
    , vcsmTemplateName :: Maybe Text
      -- ^ The name of the configuration template to validate the settings against.
      -- Condition: You cannot specify both this and an environment name.
    } deriving (Eq, Show, Generic)

instance ToQuery ValidateConfigurationSettings

instance AWSRequest ValidateConfigurationSettings where
    type Er ValidateConfigurationSettings = ElasticBeanstalkError
    type Rs ValidateConfigurationSettings = ValidateConfigurationSettingsResponse
    request = getQuery service "ValidateConfigurationSettings"

data ValidateConfigurationSettingsResponse = ValidateConfigurationSettingsResponse
    { vcsmrsMessages :: [ValidationMessage]
      -- ^ A list of ValidationMessage.
    } deriving (Eq, Show, Generic)

instance FromXML ValidateConfigurationSettingsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ValidateConfigurationSettingsResponse"
        :| ["ValidateConfigurationSettingsResult"]
