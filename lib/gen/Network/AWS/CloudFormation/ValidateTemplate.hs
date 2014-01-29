{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.ValidateTemplate
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Validates a specified template.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=ValidateTemplate
-- &TemplateBody=http://myTemplateRepository/TemplateOne.template
-- &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature] false InstanceType Type of instance to launch
-- m1.small false WebServerPort The TCP port for the Web Server 8888 false
-- KeyName Name of an existing EC2 KeyPair to enable SSH access into the
-- server 0be7b6e8-e4a0-11e0-a5bd-9f8d5a7dbc91.
module Network.AWS.CloudFormation.ValidateTemplate where

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

import Network.AWS.CloudFormation.Service
import Network.AWS.CloudFormation.Types

-- | Convenience method utilising default fields where applicable.
validateTemplate :: AWS (Either CloudFormationError ValidateTemplateResponse)
validateTemplate = undefined $ ValidateTemplate
    { vtiTemplateBody = Nothing
    , vtiTemplateURL = Nothing
    }

data ValidateTemplate = ValidateTemplate
    { vtiTemplateBody :: Maybe Text
      -- ^ String containing the template body. (For more information, go to the AWS
      -- CloudFormation User Guide.) Conditional: You must pass TemplateURL or
      -- TemplateBody. If both are passed, only TemplateBody is used.
    , vtiTemplateURL :: Maybe Text
      -- ^ Location of file containing the template body. The URL must point to a
      -- template (max size: 307,200 bytes) located in an S3 bucket in the same
      -- region as the stack. For more information, go to the AWS CloudFormation
      -- User Guide. Conditional: You must pass TemplateURL or TemplateBody. If both
      -- are passed, only TemplateBody is used.
    } deriving (Eq, Show, Generic)

instance ToQuery ValidateTemplate

instance AWSRequest ValidateTemplate where
    type Er ValidateTemplate = CloudFormationError
    type Rs ValidateTemplate = ValidateTemplateResponse
    request = getQuery service "ValidateTemplate"

data ValidateTemplateResponse = ValidateTemplateResponse
    { vtirsCapabilities :: [Capability]
      -- ^ The capabilities found within the template. Currently, CAPABILITY_IAM is
      -- the only capability detected. If your template contains IAM resources, you
      -- must specify the CAPABILITY_IAM value for this parameter when you use the
      -- CreateStack or UpdateStack actions with your template; otherwise, those
      -- actions return an InsufficientCapabilities error.
    , vtirsCapabilitiesReason :: Maybe Text
      -- ^ The capabilities reason found within the template.
    , vtirsDescription :: Maybe Text
      -- ^ The description found within the template.
    , vtirsParameters :: [TemplateParameter]
      -- ^ A list of TemplateParameter structures.
    } deriving (Eq, Show, Generic)

instance FromXML ValidateTemplateResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ValidateTemplateResponse"
        :| ["ValidateTemplateResult"]
