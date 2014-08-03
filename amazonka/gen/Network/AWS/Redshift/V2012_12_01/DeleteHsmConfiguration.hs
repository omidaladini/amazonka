{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DeleteHsmConfiguration
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified Amazon Redshift HSM configuration.
module Network.AWS.Redshift.V2012_12_01.DeleteHsmConfiguration where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

data DeleteHsmConfiguration = DeleteHsmConfiguration
    { _dhcmHsmConfigurationIdentifier :: Text
      -- ^ The identifier of the Amazon Redshift HSM configuration to be
      -- deleted.
    } deriving (Generic)

makeLenses ''DeleteHsmConfiguration

instance ToQuery DeleteHsmConfiguration where
    toQuery = genericToQuery def

data DeleteHsmConfigurationResponse = DeleteHsmConfigurationResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteHsmConfigurationResponse

instance AWSRequest DeleteHsmConfiguration where
    type Sv DeleteHsmConfiguration = Redshift
    type Rs DeleteHsmConfiguration = DeleteHsmConfigurationResponse

    request = post "DeleteHsmConfiguration"
    response _ _ = return (Right DeleteHsmConfigurationResponse)