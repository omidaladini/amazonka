{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeEnvironmentResources
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns AWS resources for this environment.
-- https://elasticbeanstalk.us-east-1.amazon.com/?EnvironmentId=e-hc8mvnayrx
-- &EnvironmentName=SampleAppVersion &Operation=DescribeEnvironmentResources
-- &AuthParams elasticbeanstalk-SampleAppVersion
-- elasticbeanstalk-SampleAppVersion-hbAc8cSZH7
-- elasticbeanstalk-SampleAppVersion-us-east-1c SampleAppVersion
-- elasticbeanstalk-SampleAppVersion-us-east-1c
-- e1cb7b96-f287-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk.V2010_12_01.DescribeEnvironmentResources where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeEnvironmentResources' request.
describeEnvironmentResources :: DescribeEnvironmentResources
describeEnvironmentResources = DescribeEnvironmentResources
    { _dermEnvironmentId = Nothing
    , _dermEnvironmentName = Nothing
    }

data DescribeEnvironmentResources = DescribeEnvironmentResources
    { _dermEnvironmentId :: Maybe Text
      -- ^ The ID of the environment to retrieve AWS resource usage data.
      -- Condition: You must specify either this or an EnvironmentName, or
      -- both. If you do not specify either, AWS Elastic Beanstalk returns
      -- MissingRequiredParameter error.
    , _dermEnvironmentName :: Maybe Text
      -- ^ The name of the environment to retrieve AWS resource usage data.
      -- Condition: You must specify either this or an EnvironmentId, or
      -- both. If you do not specify either, AWS Elastic Beanstalk returns
      -- MissingRequiredParameter error.
    } deriving (Show, Generic)

makeLenses ''DescribeEnvironmentResources

instance ToQuery DescribeEnvironmentResources where
    toQuery = genericQuery def

data DescribeEnvironmentResourcesResponse = DescribeEnvironmentResourcesResponse
    { _erdmEnvironmentResources :: Maybe EnvironmentResourceDescription
      -- ^ A list of EnvironmentResourceDescription.
    } deriving (Show, Generic)

makeLenses ''DescribeEnvironmentResourcesResponse

instance FromXML DescribeEnvironmentResourcesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeEnvironmentResources where
    type Sv DescribeEnvironmentResources = ElasticBeanstalk
    type Rs DescribeEnvironmentResources = DescribeEnvironmentResourcesResponse

    request = post "DescribeEnvironmentResources"
    response _ = xmlResponse