{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.UpdateGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the name and/or the path of the specified group. You should
-- understand the implications of changing a group's path or name. For more
-- information, see Renaming Users and Groups in the Using IAM guide. To
-- change a group name the requester must have appropriate permissions on both
-- the source object and the target object. For example, to change Managers to
-- MGRs, the entity making the request must have permission on Managers and
-- MGRs, or must have permission on all (*). For more information about
-- permissions, see Permissions and Policies. https://iam.amazonaws.com/
-- ?Action=UpdateGroup &GroupName=Test &NewGroupName=Test_1
-- &Version=2010-05-08 &AUTHPARAMS
-- /division_abc/subdivision_xyz/product_1234/engineering/ Test_1
-- AGP2MAB8DPLSRHEXAMPLE
-- arn:aws:iam::123456789012:group/division_abc/subdivision_xyz/
-- product_1234/engineering/Test_1 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.UpdateGroup
    (
    -- * Request
      UpdateGroup
    -- ** Request constructor
    , mkUpdateGroup
    -- ** Request lenses
    , ugGroupName
    , ugNewPath
    , ugNewGroupName

    -- * Response
    , UpdateGroupResponse
    -- ** Response constructor
    , mkUpdateGroupResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data UpdateGroup = UpdateGroup
    { _ugGroupName :: !Text
    , _ugNewPath :: !(Maybe Text)
    , _ugNewGroupName :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GroupName ::@ @Text@
--
-- * @NewPath ::@ @Maybe Text@
--
-- * @NewGroupName ::@ @Maybe Text@
--
mkUpdateGroup :: Text -- ^ 'ugGroupName'
              -> UpdateGroup
mkUpdateGroup p1 = UpdateGroup
    { _ugGroupName = p1
    , _ugNewPath = Nothing
    , _ugNewGroupName = Nothing
    }

-- | Name of the group to update. If you're changing the name of the group, this
-- is the original name.
ugGroupName :: Lens' UpdateGroup Text
ugGroupName = lens _ugGroupName (\s a -> s { _ugGroupName = a })

-- | New path for the group. Only include this if changing the group's path.
ugNewPath :: Lens' UpdateGroup (Maybe Text)
ugNewPath = lens _ugNewPath (\s a -> s { _ugNewPath = a })

-- | New name for the group. Only include this if changing the group's name.
ugNewGroupName :: Lens' UpdateGroup (Maybe Text)
ugNewGroupName = lens _ugNewGroupName (\s a -> s { _ugNewGroupName = a })

instance ToQuery UpdateGroup where
    toQuery = genericQuery def

data UpdateGroupResponse = UpdateGroupResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkUpdateGroupResponse :: UpdateGroupResponse
mkUpdateGroupResponse = UpdateGroupResponse

instance AWSRequest UpdateGroup where
    type Sv UpdateGroup = IAM
    type Rs UpdateGroup = UpdateGroupResponse

    request = post "UpdateGroup"
    response _ = nullaryResponse UpdateGroupResponse