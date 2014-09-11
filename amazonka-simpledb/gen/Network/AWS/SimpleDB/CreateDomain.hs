{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SimpleDB.CreateDomain
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateDomain operation creates a new domain. The domain name should be
-- unique among the domains associated with the Access Key ID provided in the
-- request. The CreateDomain operation may take 10 or more seconds to
-- complete. CreateDomain is an idempotent operation; running it multiple
-- times using the same domain name will not result in an error response. The
-- client can create up to 100 domains per account. If the client requires
-- additional domains, go to
-- http://aws.amazon.com/contact-us/simpledb-limit-request/.
module Network.AWS.SimpleDB.CreateDomain
    (
    -- * Request
      CreateDomain
    -- ** Request constructor
    , mkCreateDomain
    -- ** Request lenses
    , cdDomainName

    -- * Response
    , CreateDomainResponse
    -- ** Response constructor
    , mkCreateDomainResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SimpleDB.Types
import Network.AWS.Prelude

newtype CreateDomain = CreateDomain
    { _cdDomainName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateDomain' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DomainName ::@ @Text@
--
mkCreateDomain :: Text -- ^ 'cdDomainName'
               -> CreateDomain
mkCreateDomain p1 = CreateDomain
    { _cdDomainName = p1
    }

-- | The name of the domain to create. The name can range between 3 and 255
-- characters and can contain the following characters: a-z, A-Z, 0-9, '_',
-- '-', and '.'.
cdDomainName :: Lens' CreateDomain Text
cdDomainName = lens _cdDomainName (\s a -> s { _cdDomainName = a })

instance ToQuery CreateDomain where
    toQuery = genericQuery def

data CreateDomainResponse = CreateDomainResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateDomainResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkCreateDomainResponse :: CreateDomainResponse
mkCreateDomainResponse = CreateDomainResponse

instance AWSRequest CreateDomain where
    type Sv CreateDomain = SimpleDB
    type Rs CreateDomain = CreateDomainResponse

    request = post "CreateDomain"
    response _ = nullaryResponse CreateDomainResponse