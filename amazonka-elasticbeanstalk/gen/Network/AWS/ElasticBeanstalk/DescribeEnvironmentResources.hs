{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources
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
module Network.AWS.ElasticBeanstalk.DescribeEnvironmentResources
    (
    -- * Request
      DescribeEnvironmentResources
    -- ** Request constructor
    , mkDescribeEnvironmentResources
    -- ** Request lenses
    , derEnvironmentId
    , derEnvironmentName

    -- * Response
    , DescribeEnvironmentResourcesResponse
    -- ** Response constructor
    , mkDescribeEnvironmentResourcesResponse
    -- ** Response lenses
    , derrEnvironmentResources
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude

-- | This documentation target is not reported in the API reference.
data DescribeEnvironmentResources = DescribeEnvironmentResources
    { _derEnvironmentId :: !(Maybe Text)
    , _derEnvironmentName :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeEnvironmentResources' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @EnvironmentId ::@ @Maybe Text@
--
-- * @EnvironmentName ::@ @Maybe Text@
--
mkDescribeEnvironmentResources :: DescribeEnvironmentResources
mkDescribeEnvironmentResources = DescribeEnvironmentResources
    { _derEnvironmentId = Nothing
    , _derEnvironmentName = Nothing
    }

-- | The ID of the environment to retrieve AWS resource usage data. Condition:
-- You must specify either this or an EnvironmentName, or both. If you do not
-- specify either, AWS Elastic Beanstalk returns MissingRequiredParameter
-- error.
derEnvironmentId :: Lens' DescribeEnvironmentResources (Maybe Text)
derEnvironmentId =
    lens _derEnvironmentId (\s a -> s { _derEnvironmentId = a })

-- | The name of the environment to retrieve AWS resource usage data. Condition:
-- You must specify either this or an EnvironmentId, or both. If you do not
-- specify either, AWS Elastic Beanstalk returns MissingRequiredParameter
-- error.
derEnvironmentName :: Lens' DescribeEnvironmentResources (Maybe Text)
derEnvironmentName =
    lens _derEnvironmentName (\s a -> s { _derEnvironmentName = a })

instance ToQuery DescribeEnvironmentResources where
    toQuery = genericQuery def

-- | Result message containing a list of environment resource descriptions.
newtype DescribeEnvironmentResourcesResponse = DescribeEnvironmentResourcesResponse
    { _derrEnvironmentResources :: Maybe EnvironmentResourceDescription
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeEnvironmentResourcesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @EnvironmentResources ::@ @Maybe EnvironmentResourceDescription@
--
mkDescribeEnvironmentResourcesResponse :: DescribeEnvironmentResourcesResponse
mkDescribeEnvironmentResourcesResponse = DescribeEnvironmentResourcesResponse
    { _derrEnvironmentResources = Nothing
    }

-- | A list of EnvironmentResourceDescription.
derrEnvironmentResources :: Lens' DescribeEnvironmentResourcesResponse (Maybe EnvironmentResourceDescription)
derrEnvironmentResources =
    lens _derrEnvironmentResources
         (\s a -> s { _derrEnvironmentResources = a })

instance FromXML DescribeEnvironmentResourcesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeEnvironmentResources where
    type Sv DescribeEnvironmentResources = ElasticBeanstalk
    type Rs DescribeEnvironmentResources = DescribeEnvironmentResourcesResponse

    request = post "DescribeEnvironmentResources"
    response _ = xmlResponse