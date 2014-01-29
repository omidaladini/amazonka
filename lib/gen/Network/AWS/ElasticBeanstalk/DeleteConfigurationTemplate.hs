{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.DeleteConfigurationTemplate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified configuration template. When you launch an
-- environment using a configuration template, the environment gets a copy of
-- the template. You can delete or modify the environment's copy of the
-- template without affecting the running environment.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationName=SampleApp
-- &TemplateName=SampleAppTemplate &Operation=DeleteConfigurationTemplate
-- &AuthParams af9cf1b6-f25e-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.DeleteConfigurationTemplate where

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

-- | Convenience method utilising default fields where applicable.
deleteConfigurationTemplate :: Text
                            -> Text
                            -> AWS (Either ElasticBeanstalkError DeleteConfigurationTemplateResponse)
deleteConfigurationTemplate p1 p2 = undefined $ DeleteConfigurationTemplate
    { dctmApplicationName = p1
    , dctmTemplateName = p2
    }

data DeleteConfigurationTemplate = DeleteConfigurationTemplate
    { dctmApplicationName :: !Text
      -- ^ The name of the application to delete the configuration template from.
    , dctmTemplateName :: !Text
      -- ^ The name of the configuration template to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteConfigurationTemplate

instance AWSRequest DeleteConfigurationTemplate where
    type Er DeleteConfigurationTemplate = ElasticBeanstalkError
    type Rs DeleteConfigurationTemplate = DeleteConfigurationTemplateResponse
    request = getQuery service "DeleteConfigurationTemplate"

data DeleteConfigurationTemplateResponse = DeleteConfigurationTemplateResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteConfigurationTemplateResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeleteConfigurationTemplateResponse"
        :| ["DeleteConfigurationTemplateResult"]
