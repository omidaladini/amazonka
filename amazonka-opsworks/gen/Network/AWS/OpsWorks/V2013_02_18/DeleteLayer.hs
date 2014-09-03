{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DeleteLayer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a specified layer. You must first stop and then delete all
-- associated instances. For more information, see How to Delete a Layer.
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly
-- grants permissions. For more information on user permissions, see Managing
-- User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.DeleteLayer
    (
    -- * Request
      DeleteLayer
    -- ** Request constructor
    , deleteLayer
    -- ** Request lenses
    , dlrLayerId

    -- * Response
    , DeleteLayerResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DeleteLayer' request.
deleteLayer :: Text -- ^ 'dlrLayerId'
            -> DeleteLayer
deleteLayer p1 = DeleteLayer
    { _dlrLayerId = p1
    }

data DeleteLayer = DeleteLayer
    { _dlrLayerId :: Text
      -- ^ The layer ID.
    } deriving (Show, Generic)

-- | The layer ID.
dlrLayerId
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteLayer
    -> f DeleteLayer
dlrLayerId f x =
    (\y -> x { _dlrLayerId = y })
       <$> f (_dlrLayerId x)
{-# INLINE dlrLayerId #-}

instance ToPath DeleteLayer

instance ToQuery DeleteLayer

instance ToHeaders DeleteLayer

instance ToJSON DeleteLayer

data DeleteLayerResponse = DeleteLayerResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteLayer where
    type Sv DeleteLayer = OpsWorks
    type Rs DeleteLayer = DeleteLayerResponse

    request = get
    response _ = nullaryResponse DeleteLayerResponse
