{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DeleteHsmConfiguration
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified Amazon Redshift HSM configuration.
module Network.AWS.Redshift.DeleteHsmConfiguration
    (
    -- * Request
      DeleteHsmConfiguration
    -- ** Request constructor
    , mkDeleteHsmConfiguration
    -- ** Request lenses
    , dhcHsmConfigurationIdentifier

    -- * Response
    , DeleteHsmConfigurationResponse
    -- ** Response constructor
    , mkDeleteHsmConfigurationResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import Network.AWS.Prelude

-- | 
newtype DeleteHsmConfiguration = DeleteHsmConfiguration
    { _dhcHsmConfigurationIdentifier :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteHsmConfiguration' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @HsmConfigurationIdentifier ::@ @Text@
--
mkDeleteHsmConfiguration :: Text -- ^ 'dhcHsmConfigurationIdentifier'
                         -> DeleteHsmConfiguration
mkDeleteHsmConfiguration p1 = DeleteHsmConfiguration
    { _dhcHsmConfigurationIdentifier = p1
    }

-- | The identifier of the Amazon Redshift HSM configuration to be deleted.
dhcHsmConfigurationIdentifier :: Lens' DeleteHsmConfiguration Text
dhcHsmConfigurationIdentifier =
    lens _dhcHsmConfigurationIdentifier
         (\s a -> s { _dhcHsmConfigurationIdentifier = a })

instance ToQuery DeleteHsmConfiguration where
    toQuery = genericQuery def

data DeleteHsmConfigurationResponse = DeleteHsmConfigurationResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteHsmConfigurationResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteHsmConfigurationResponse :: DeleteHsmConfigurationResponse
mkDeleteHsmConfigurationResponse = DeleteHsmConfigurationResponse

instance AWSRequest DeleteHsmConfiguration where
    type Sv DeleteHsmConfiguration = Redshift
    type Rs DeleteHsmConfiguration = DeleteHsmConfigurationResponse

    request = post "DeleteHsmConfiguration"
    response _ = nullaryResponse DeleteHsmConfigurationResponse