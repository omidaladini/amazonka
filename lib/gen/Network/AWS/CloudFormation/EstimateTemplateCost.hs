{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.EstimateTemplateCost
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the estimated monthly cost of a template. The return value is an
-- AWS Simple Monthly Calculator URL with a query string that describes the
-- resources required to run the template.
-- https://cloudformation.us-east-1.amazonaws.com/
-- ?Action=EstimateTemplateCost &TemplateURL=
-- https://s3.amazonaws.com/cloudformation-samples-us-east-1/Drupal_Simple.template
-- &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2011-12-04T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature]
-- http://calculator.s3.amazonaws.com/calc5.html?key=cf-2e351785-e821-450c-9d58-625e1e1ebfb6.
-- FIXME: Operation documentation for EstimateTemplateCost
module Network.AWS.CloudFormation.EstimateTemplateCost where

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
estimateTemplateCost :: AWS (Either CloudFormationError EstimateTemplateCostResponse)
estimateTemplateCost = undefined $ EstimateTemplateCost
    { etciParameters = []
    , etciTemplateBody = Nothing
    , etciTemplateURL = Nothing
    }

data EstimateTemplateCost = EstimateTemplateCost
    { etciParameters :: [Parameter]
      -- ^ A list of Parameter structures that specify input parameters.
    , etciTemplateBody :: Maybe Text
      -- ^ Structure containing the template body. (For more information, go to the
      -- AWS CloudFormation User Guide.) Conditional: You must pass TemplateBody or
      -- TemplateURL. If both are passed, only TemplateBody is used.
    , etciTemplateURL :: Maybe Text
      -- ^ Location of file containing the template body. The URL must point to a
      -- template located in an S3 bucket in the same region as the stack. For more
      -- information, go to the AWS CloudFormation User Guide. Conditional: You must
      -- pass TemplateURL or TemplateBody. If both are passed, only TemplateBody is
      -- used.
    } deriving (Eq, Show, Generic)

instance ToQuery EstimateTemplateCost

instance AWSRequest EstimateTemplateCost where
    type Er EstimateTemplateCost = CloudFormationError
    type Rs EstimateTemplateCost = EstimateTemplateCostResponse
    request = getQuery service "EstimateTemplateCost"

data EstimateTemplateCostResponse = EstimateTemplateCostResponse
    { etcirsUrl :: Maybe Text
      -- ^ An AWS Simple Monthly Calculator URL with a query string that describes the
      -- resources required to run the template.
    } deriving (Eq, Show, Generic)

instance FromXML EstimateTemplateCostResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "EstimateTemplateCostResponse"
        :| ["EstimateTemplateCostResult"]
