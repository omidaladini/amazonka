{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ListSAMLProviders
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the SAML providers in the account. This operation requires Signature
-- Version 4. https://iam.amazonaws.com/ ?Action=ListSAMLProviders
-- &MaxItems=100 &PathPrefix=/application_abc/ &Version=2010-05-08 &AUTHPARAMS
-- arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Database
-- 2032-05-09T16:27:11Z 2012-05-09T16:27:03Z
-- arn:aws:iam::123456789012:instance-profile/application_abc/component_xyz/Webserver
-- 2015-03-11T13:11:02Z 2012-05-09T16:27:11Z
-- fd74fa8d-99f3-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.ListSAMLProviders where

import Network.AWS.Core
import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

data ListSAMLProviders = ListSAMLProviders
    deriving (Eq, Show, Generic)

instance ToQuery ListSAMLProviders

instance AWSRequest ListSAMLProviders where
    type Er ListSAMLProviders = IAMError
    type Rs ListSAMLProviders = ListSAMLProvidersResponse
    request = getQuery service "ListSAMLProviders"

data ListSAMLProvidersResponse = ListSAMLProvidersResponse
    { lsamlprrSAMLProviderList :: [SAMLProviderListEntry]
      -- ^ The list of SAML providers for this account.
    } deriving (Eq, Show, Generic)

instance FromXML ListSAMLProvidersResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ListSAMLProvidersResponse"
        :| ["ListSAMLProvidersResult"]
