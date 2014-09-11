{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
-- 
module Network.AWS.CloudFormation.EstimateTemplateCost
    (
    -- * Request
      EstimateTemplateCost
    -- ** Request constructor
    , mkEstimateTemplateCost
    -- ** Request lenses
    , etcTemplateBody
    , etcTemplateURL
    , etcParameters

    -- * Response
    , EstimateTemplateCostResponse
    -- ** Response constructor
    , mkEstimateTemplateCostResponse
    -- ** Response lenses
    , etcrUrl
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types
import Network.AWS.Prelude

data EstimateTemplateCost = EstimateTemplateCost
    { _etcTemplateBody :: !(Maybe Text)
    , _etcTemplateURL :: !(Maybe Text)
    , _etcParameters :: [Parameter]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'EstimateTemplateCost' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TemplateBody ::@ @Maybe Text@
--
-- * @TemplateURL ::@ @Maybe Text@
--
-- * @Parameters ::@ @[Parameter]@
--
mkEstimateTemplateCost :: EstimateTemplateCost
mkEstimateTemplateCost = EstimateTemplateCost
    { _etcTemplateBody = Nothing
    , _etcTemplateURL = Nothing
    , _etcParameters = mempty
    }

-- | Structure containing the template body with a minimum length of 1 byte and
-- a maximum length of 51,200 bytes. (For more information, go to Template
-- Anatomy in the AWS CloudFormation User Guide.) Conditional: You must pass
-- TemplateBody or TemplateURL. If both are passed, only TemplateBody is used.
etcTemplateBody :: Lens' EstimateTemplateCost (Maybe Text)
etcTemplateBody = lens _etcTemplateBody (\s a -> s { _etcTemplateBody = a })

-- | Location of file containing the template body. The URL must point to a
-- template located in an S3 bucket in the same region as the stack. For more
-- information, go to Template Anatomy in the AWS CloudFormation User Guide.
-- Conditional: You must pass TemplateURL or TemplateBody. If both are passed,
-- only TemplateBody is used.
etcTemplateURL :: Lens' EstimateTemplateCost (Maybe Text)
etcTemplateURL = lens _etcTemplateURL (\s a -> s { _etcTemplateURL = a })

-- | A list of Parameter structures that specify input parameters.
etcParameters :: Lens' EstimateTemplateCost [Parameter]
etcParameters = lens _etcParameters (\s a -> s { _etcParameters = a })

instance ToQuery EstimateTemplateCost where
    toQuery = genericQuery def

-- | The output for a EstimateTemplateCost action.
newtype EstimateTemplateCostResponse = EstimateTemplateCostResponse
    { _etcrUrl :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'EstimateTemplateCostResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Url ::@ @Maybe Text@
--
mkEstimateTemplateCostResponse :: EstimateTemplateCostResponse
mkEstimateTemplateCostResponse = EstimateTemplateCostResponse
    { _etcrUrl = Nothing
    }

-- | An AWS Simple Monthly Calculator URL with a query string that describes the
-- resources required to run the template.
etcrUrl :: Lens' EstimateTemplateCostResponse (Maybe Text)
etcrUrl = lens _etcrUrl (\s a -> s { _etcrUrl = a })

instance FromXML EstimateTemplateCostResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest EstimateTemplateCost where
    type Sv EstimateTemplateCost = CloudFormation
    type Rs EstimateTemplateCost = EstimateTemplateCostResponse

    request = post "EstimateTemplateCost"
    response _ = xmlResponse