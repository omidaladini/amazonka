{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DescribePermissions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the permissions for a specified stack. Required Permissions: To
-- use this action, an IAM user must have a Manage permissions level for the
-- stack, or an attached policy that explicitly grants permissions. For more
-- information on user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.DescribePermissions
    (
    -- * Request
      DescribePermissions
    -- ** Request constructor
    , describePermissions
    -- ** Request lenses
    , dprIamUserArn
    , dprStackId

    -- * Response
    , DescribePermissionsResponse
    -- ** Response lenses
    , dpsPermissions
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribePermissions' request.
describePermissions :: DescribePermissions
describePermissions = DescribePermissions
    { _dprIamUserArn = Nothing
    , _dprStackId = Nothing
    }

data DescribePermissions = DescribePermissions
    { _dprIamUserArn :: Maybe Text
      -- ^ The user's IAM ARN. For more information about IAM ARNs, see
      -- Using Identifiers.
    , _dprStackId :: Maybe Text
      -- ^ The stack ID.
    } deriving (Show, Generic)

-- | The user's IAM ARN. For more information about IAM ARNs, see Using
-- Identifiers.
dprIamUserArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribePermissions
    -> f DescribePermissions
dprIamUserArn f x =
    (\y -> x { _dprIamUserArn = y })
       <$> f (_dprIamUserArn x)
{-# INLINE dprIamUserArn #-}

-- | The stack ID.
dprStackId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribePermissions
    -> f DescribePermissions
dprStackId f x =
    (\y -> x { _dprStackId = y })
       <$> f (_dprStackId x)
{-# INLINE dprStackId #-}

instance ToPath DescribePermissions

instance ToQuery DescribePermissions

instance ToHeaders DescribePermissions

instance ToJSON DescribePermissions

data DescribePermissionsResponse = DescribePermissionsResponse
    { _dpsPermissions :: [Permission]
      -- ^ An array of Permission objects that describe the stack
      -- permissions. If the request object contains only a stack ID, the
      -- array contains a Permission object with permissions for each of
      -- the stack IAM ARNs. If the request object contains only an IAM
      -- ARN, the array contains a Permission object with permissions for
      -- each of the user's stack IDs. If the request contains a stack ID
      -- and an IAM ARN, the array contains a single Permission object
      -- with permissions for the specified stack and IAM ARN.
    } deriving (Show, Generic)

-- | An array of Permission objects that describe the stack permissions. If the
-- request object contains only a stack ID, the array contains a Permission
-- object with permissions for each of the stack IAM ARNs. If the request
-- object contains only an IAM ARN, the array contains a Permission object
-- with permissions for each of the user's stack IDs. If the request contains
-- a stack ID and an IAM ARN, the array contains a single Permission object
-- with permissions for the specified stack and IAM ARN.
dpsPermissions
    :: Functor f
    => ([Permission]
    -> f ([Permission]))
    -> DescribePermissionsResponse
    -> f DescribePermissionsResponse
dpsPermissions f x =
    (\y -> x { _dpsPermissions = y })
       <$> f (_dpsPermissions x)
{-# INLINE dpsPermissions #-}

instance FromJSON DescribePermissionsResponse

instance AWSRequest DescribePermissions where
    type Sv DescribePermissions = OpsWorks
    type Rs DescribePermissions = DescribePermissionsResponse

    request = get
    response _ = jsonResponse
