{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.DescribeServiceAccessPolicies
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets information about the access policies that control access to the
-- domain's document and search endpoints. By default, shows the configuration
-- with any pending changes. Set the Deployed option to true to show the
-- active configuration and exclude pending changes. For more information, see
-- Configuring Access for a Search Domain in the Amazon CloudSearch Developer
-- Guide.
module Network.AWS.CloudSearch.V2013_01_01.DescribeServiceAccessPolicies where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeServiceAccessPolicies' request.
describeServiceAccessPolicies :: Text -- ^ '_dsaprDomainName'
                              -> DescribeServiceAccessPolicies
describeServiceAccessPolicies p1 = DescribeServiceAccessPolicies
    { _dsaprDomainName = p1
    , _dsaprDeployed = Nothing
    }

data DescribeServiceAccessPolicies = DescribeServiceAccessPolicies
    { _dsaprDomainName :: Text
      -- ^ The name of the domain you want to describe.
    , _dsaprDeployed :: Maybe Bool
      -- ^ Whether to display the deployed configuration (true) or include
      -- any pending changes (false). Defaults to false.
    } deriving (Show, Generic)

makeLenses ''DescribeServiceAccessPolicies

instance ToQuery DescribeServiceAccessPolicies where
    toQuery = genericQuery def

data DescribeServiceAccessPoliciesResponse = DescribeServiceAccessPoliciesResponse
    { _dsapsAccessPolicies :: AccessPoliciesStatus
      -- ^ The access rules configured for the domain specified in the
      -- request.
    } deriving (Show, Generic)

makeLenses ''DescribeServiceAccessPoliciesResponse

instance FromXML DescribeServiceAccessPoliciesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeServiceAccessPolicies where
    type Sv DescribeServiceAccessPolicies = CloudSearch
    type Rs DescribeServiceAccessPolicies = DescribeServiceAccessPoliciesResponse

    request = post "DescribeServiceAccessPolicies"
    response _ = xmlResponse