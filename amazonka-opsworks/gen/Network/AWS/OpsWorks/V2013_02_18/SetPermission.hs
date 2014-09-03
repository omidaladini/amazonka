{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.SetPermission
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Specifies a user's permissions. For more information, see Security and
-- Permissions. Required Permissions: To use this action, an IAM user must
-- have a Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.SetPermission
    (
    -- * Request
      SetPermission
    -- ** Request constructor
    , setPermission
    -- ** Request lenses
    , sprStackId
    , sprIamUserArn
    , sprAllowSsh
    , sprAllowSudo
    , sprLevel

    -- * Response
    , SetPermissionResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'SetPermission' request.
setPermission :: Text -- ^ 'sprStackId'
              -> Text -- ^ 'sprIamUserArn'
              -> SetPermission
setPermission p1 p2 = SetPermission
    { _sprStackId = p1
    , _sprIamUserArn = p2
    , _sprAllowSsh = Nothing
    , _sprAllowSudo = Nothing
    , _sprLevel = Nothing
    }

data SetPermission = SetPermission
    { _sprStackId :: Text
      -- ^ The stack ID.
    , _sprIamUserArn :: Text
      -- ^ The user's IAM ARN.
    , _sprAllowSsh :: Maybe Bool
      -- ^ The user is allowed to use SSH to communicate with the instance.
    , _sprAllowSudo :: Maybe Bool
      -- ^ The user is allowed to use sudo to elevate privileges.
    , _sprLevel :: Maybe Text
      -- ^ The user's permission level, which must be set to one of the
      -- following strings. You cannot set your own permissions level.
      -- deny show deploy manage iam_only For more information on the
      -- permissions associated with these levels, see Managing User
      -- Permissions.
    } deriving (Show, Generic)

-- | The stack ID.
sprStackId
    :: Functor f
    => (Text
    -> f (Text))
    -> SetPermission
    -> f SetPermission
sprStackId f x =
    (\y -> x { _sprStackId = y })
       <$> f (_sprStackId x)
{-# INLINE sprStackId #-}

-- | The user's IAM ARN.
sprIamUserArn
    :: Functor f
    => (Text
    -> f (Text))
    -> SetPermission
    -> f SetPermission
sprIamUserArn f x =
    (\y -> x { _sprIamUserArn = y })
       <$> f (_sprIamUserArn x)
{-# INLINE sprIamUserArn #-}

-- | The user is allowed to use SSH to communicate with the instance.
sprAllowSsh
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> SetPermission
    -> f SetPermission
sprAllowSsh f x =
    (\y -> x { _sprAllowSsh = y })
       <$> f (_sprAllowSsh x)
{-# INLINE sprAllowSsh #-}

-- | The user is allowed to use sudo to elevate privileges.
sprAllowSudo
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> SetPermission
    -> f SetPermission
sprAllowSudo f x =
    (\y -> x { _sprAllowSudo = y })
       <$> f (_sprAllowSudo x)
{-# INLINE sprAllowSudo #-}

-- | The user's permission level, which must be set to one of the following
-- strings. You cannot set your own permissions level. deny show deploy manage
-- iam_only For more information on the permissions associated with these
-- levels, see Managing User Permissions.
sprLevel
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SetPermission
    -> f SetPermission
sprLevel f x =
    (\y -> x { _sprLevel = y })
       <$> f (_sprLevel x)
{-# INLINE sprLevel #-}

instance ToPath SetPermission

instance ToQuery SetPermission

instance ToHeaders SetPermission

instance ToJSON SetPermission

data SetPermissionResponse = SetPermissionResponse
    deriving (Eq, Show, Generic)

instance AWSRequest SetPermission where
    type Sv SetPermission = OpsWorks
    type Rs SetPermission = SetPermissionResponse

    request = get
    response _ = nullaryResponse SetPermissionResponse
