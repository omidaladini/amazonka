{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.AttachInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Attaches one or more Amazon EC2 instances to an existing Auto Scaling
-- group. After the instance(s) is attached, it becomes a part of the Auto
-- Scaling group. For more information, see Attach Amazon EC2 Instances to
-- Your Existing Auto Scaling Group in the Auto Scaling Developer Guide.
module Network.AWS.AutoScaling.V2011_01_01.AttachInstances
    (
    -- * Request
      AttachInstances
    -- ** Request constructor
    , attachInstances
    -- ** Request lenses
    , aiqAutoScalingGroupName
    , aiqInstanceIds

    -- * Response
    , AttachInstancesResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'AttachInstances' request.
attachInstances :: Text -- ^ 'aiqAutoScalingGroupName'
                -> AttachInstances
attachInstances p1 = AttachInstances
    { _aiqAutoScalingGroupName = p1
    , _aiqInstanceIds = mempty
    }

data AttachInstances = AttachInstances
    { _aiqAutoScalingGroupName :: Text
      -- ^ The name of the Auto Scaling group to which to attach the
      -- specified instance(s).
    , _aiqInstanceIds :: [Text]
      -- ^ One or more IDs of the Amazon EC2 instances to attach to the
      -- specified Auto Scaling group. You must specify at least one
      -- instance ID.
    } deriving (Show, Generic)

-- | The name of the Auto Scaling group to which to attach the specified
-- instance(s).
aiqAutoScalingGroupName
    :: Functor f
    => (Text
    -> f (Text))
    -> AttachInstances
    -> f AttachInstances
aiqAutoScalingGroupName f x =
    (\y -> x { _aiqAutoScalingGroupName = y })
       <$> f (_aiqAutoScalingGroupName x)
{-# INLINE aiqAutoScalingGroupName #-}

-- | One or more IDs of the Amazon EC2 instances to attach to the specified Auto
-- Scaling group. You must specify at least one instance ID.
aiqInstanceIds
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> AttachInstances
    -> f AttachInstances
aiqInstanceIds f x =
    (\y -> x { _aiqInstanceIds = y })
       <$> f (_aiqInstanceIds x)
{-# INLINE aiqInstanceIds #-}

instance ToQuery AttachInstances where
    toQuery = genericQuery def

data AttachInstancesResponse = AttachInstancesResponse
    deriving (Eq, Show, Generic)

instance AWSRequest AttachInstances where
    type Sv AttachInstances = AutoScaling
    type Rs AttachInstances = AttachInstancesResponse

    request = post "AttachInstances"
    response _ = nullaryResponse AttachInstancesResponse
