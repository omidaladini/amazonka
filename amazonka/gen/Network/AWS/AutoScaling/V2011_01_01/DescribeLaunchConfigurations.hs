{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DescribeLaunchConfigurations
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a full description of the launch configurations, or the specified
-- launch configurations, if they exist. If no name is specified, then the
-- full details of all launch configurations are returned.
-- https://autoscaling.amazonaws.com/?LaunchConfigurationNames.member.1=my-test-lc
-- &MaxRecords=20 &Version=2011-01-01 &Action=DescribeLaunchConfigurations
-- &AUTHPARAMS true dedicated 2013-01-21T23:04:42.200Z my-test-lc m1.small
-- arn:aws:autoscaling:us-east-1:803981987763:launchConfiguration:
-- 9dbbbf87-6141-428a-a409-0752edbe6cad:launchConfigurationName/my-test-lc
-- ami-514ac838 true false d05a22f8-b690-11e2-bf8e-2113fEXAMPLE.
module Network.AWS.AutoScaling.V2011_01_01.DescribeLaunchConfigurations where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeLaunchConfigurations' request.
describeLaunchConfigurations :: DescribeLaunchConfigurations
describeLaunchConfigurations = DescribeLaunchConfigurations
    { _lcnuLaunchConfigurationNames = mempty
    , _lcnuMaxRecords = Nothing
    , _lcnuNextToken = Nothing
    }

data DescribeLaunchConfigurations = DescribeLaunchConfigurations
    { _lcnuLaunchConfigurationNames :: [Text]
      -- ^ A list of launch configuration names.
    , _lcnuMaxRecords :: Maybe Integer
      -- ^ The maximum number of launch configurations. The default is 100.
    , _lcnuNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    } deriving (Show, Generic)

makeLenses ''DescribeLaunchConfigurations

instance ToQuery DescribeLaunchConfigurations where
    toQuery = genericToQuery def

data DescribeLaunchConfigurationsResponse = DescribeLaunchConfigurationsResponse
    { _lctLaunchConfigurations :: [LaunchConfiguration]
      -- ^ A list of launch configurations.
    , _lctNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    } deriving (Show, Generic)

makeLenses ''DescribeLaunchConfigurationsResponse

instance AWSRequest DescribeLaunchConfigurations where
    type Sv DescribeLaunchConfigurations = AutoScaling
    type Rs DescribeLaunchConfigurations = DescribeLaunchConfigurationsResponse

    request = post "DescribeLaunchConfigurations"
    response _ = cursorResponse $ \hs xml ->
        pure DescribeLaunchConfigurationsResponse
            <*> xml %| "LaunchConfigurations"
            <*> xml %|? "XmlString"

instance AWSPager DescribeLaunchConfigurations where
    next rq rs = (\x -> rq { _lcnuNextToken = Just x })
        <$> (_lctNextToken rs)