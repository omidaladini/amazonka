{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.UpdateLoginProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Changes the password for the specified user. https://iam.amazonaws.com/
-- ?Action=UpdateLoginProfile &UserName=Bob &Password=NewPassword &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.UpdateLoginProfile
    (
    -- * Request
      UpdateLoginProfile
    -- ** Request constructor
    , updateLoginProfile
    -- ** Request lenses
    , ulprUserName
    , ulprPasswordResetRequired
    , ulprPassword

    -- * Response
    , UpdateLoginProfileResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UpdateLoginProfile' request.
updateLoginProfile :: Text -- ^ 'ulprUserName'
                   -> UpdateLoginProfile
updateLoginProfile p1 = UpdateLoginProfile
    { _ulprUserName = p1
    , _ulprPasswordResetRequired = Nothing
    , _ulprPassword = Nothing
    }

data UpdateLoginProfile = UpdateLoginProfile
    { _ulprUserName :: Text
      -- ^ Name of the user whose password you want to update.
    , _ulprPasswordResetRequired :: Maybe Bool
      -- ^ Require the specified user to set a new password on next sign-in.
    , _ulprPassword :: Maybe Text
      -- ^ The new password for the specified user.
    } deriving (Show, Generic)

-- | Name of the user whose password you want to update.
ulprUserName
    :: Functor f
    => (Text
    -> f (Text))
    -> UpdateLoginProfile
    -> f UpdateLoginProfile
ulprUserName f x =
    (\y -> x { _ulprUserName = y })
       <$> f (_ulprUserName x)
{-# INLINE ulprUserName #-}

-- | Require the specified user to set a new password on next sign-in.
ulprPasswordResetRequired
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> UpdateLoginProfile
    -> f UpdateLoginProfile
ulprPasswordResetRequired f x =
    (\y -> x { _ulprPasswordResetRequired = y })
       <$> f (_ulprPasswordResetRequired x)
{-# INLINE ulprPasswordResetRequired #-}

-- | The new password for the specified user.
ulprPassword
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateLoginProfile
    -> f UpdateLoginProfile
ulprPassword f x =
    (\y -> x { _ulprPassword = y })
       <$> f (_ulprPassword x)
{-# INLINE ulprPassword #-}

instance ToQuery UpdateLoginProfile where
    toQuery = genericQuery def

data UpdateLoginProfileResponse = UpdateLoginProfileResponse
    deriving (Eq, Show, Generic)

instance AWSRequest UpdateLoginProfile where
    type Sv UpdateLoginProfile = IAM
    type Rs UpdateLoginProfile = UpdateLoginProfileResponse

    request = post "UpdateLoginProfile"
    response _ = nullaryResponse UpdateLoginProfileResponse
