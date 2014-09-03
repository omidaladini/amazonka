{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DeregisterElasticIp
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deregisters a specified Elastic IP address. The address can then be
-- registered by another stack. For more information, see Resource Management.
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly
-- grants permissions. For more information on user permissions, see Managing
-- User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.DeregisterElasticIp
    (
    -- * Request
      DeregisterElasticIp
    -- ** Request constructor
    , deregisterElasticIp
    -- ** Request lenses
    , deirElasticIp

    -- * Response
    , DeregisterElasticIpResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DeregisterElasticIp' request.
deregisterElasticIp :: Text -- ^ 'deirElasticIp'
                    -> DeregisterElasticIp
deregisterElasticIp p1 = DeregisterElasticIp
    { _deirElasticIp = p1
    }

data DeregisterElasticIp = DeregisterElasticIp
    { _deirElasticIp :: Text
      -- ^ The Elastic IP address.
    } deriving (Show, Generic)

-- | The Elastic IP address.
deirElasticIp
    :: Functor f
    => (Text
    -> f (Text))
    -> DeregisterElasticIp
    -> f DeregisterElasticIp
deirElasticIp f x =
    (\y -> x { _deirElasticIp = y })
       <$> f (_deirElasticIp x)
{-# INLINE deirElasticIp #-}

instance ToPath DeregisterElasticIp

instance ToQuery DeregisterElasticIp

instance ToHeaders DeregisterElasticIp

instance ToJSON DeregisterElasticIp

data DeregisterElasticIpResponse = DeregisterElasticIpResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeregisterElasticIp where
    type Sv DeregisterElasticIp = OpsWorks
    type Rs DeregisterElasticIp = DeregisterElasticIpResponse

    request = get
    response _ = nullaryResponse DeregisterElasticIpResponse
