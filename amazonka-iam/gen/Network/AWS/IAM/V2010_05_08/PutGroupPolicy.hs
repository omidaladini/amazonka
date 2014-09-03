{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.PutGroupPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds (or updates) a policy document associated with the specified group.
-- For information about policies, refer to Overview of Policies in the Using
-- IAM guide. For information about limits on the number of policies you can
-- associate with a group, see Limitations on IAM Entities in the Using IAM
-- guide. Because policy documents can be large, you should use POST rather
-- than GET when calling PutGroupPolicy. For information about setting up
-- signatures and authorization through the API, go to Signing AWS API
-- Requests in the AWS General Reference. For general information about using
-- the Query API with IAM, go to Making Query Requests in the Using IAM guide.
-- https://iam.amazonaws.com/ ?Action=PutGroupPolicy &GroupName=Admins
-- &PolicyName=AdminRoot
-- &PolicyDocument={"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":"*","Resource":"*"}]}
-- &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.PutGroupPolicy
    (
    -- * Request
      PutGroupPolicy
    -- ** Request constructor
    , putGroupPolicy
    -- ** Request lenses
    , pgprGroupName
    , pgprPolicyDocument
    , pgprPolicyName

    -- * Response
    , PutGroupPolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'PutGroupPolicy' request.
putGroupPolicy :: Text -- ^ 'pgprGroupName'
               -> Text -- ^ 'pgprPolicyDocument'
               -> Text -- ^ 'pgprPolicyName'
               -> PutGroupPolicy
putGroupPolicy p1 p2 p3 = PutGroupPolicy
    { _pgprGroupName = p1
    , _pgprPolicyDocument = p2
    , _pgprPolicyName = p3
    }

data PutGroupPolicy = PutGroupPolicy
    { _pgprGroupName :: Text
      -- ^ Name of the group to associate the policy with.
    , _pgprPolicyDocument :: Text
      -- ^ The policy document.
    , _pgprPolicyName :: Text
      -- ^ Name of the policy document.
    } deriving (Show, Generic)

-- | Name of the group to associate the policy with.
pgprGroupName
    :: Functor f
    => (Text
    -> f (Text))
    -> PutGroupPolicy
    -> f PutGroupPolicy
pgprGroupName f x =
    (\y -> x { _pgprGroupName = y })
       <$> f (_pgprGroupName x)
{-# INLINE pgprGroupName #-}

-- | The policy document.
pgprPolicyDocument
    :: Functor f
    => (Text
    -> f (Text))
    -> PutGroupPolicy
    -> f PutGroupPolicy
pgprPolicyDocument f x =
    (\y -> x { _pgprPolicyDocument = y })
       <$> f (_pgprPolicyDocument x)
{-# INLINE pgprPolicyDocument #-}

-- | Name of the policy document.
pgprPolicyName
    :: Functor f
    => (Text
    -> f (Text))
    -> PutGroupPolicy
    -> f PutGroupPolicy
pgprPolicyName f x =
    (\y -> x { _pgprPolicyName = y })
       <$> f (_pgprPolicyName x)
{-# INLINE pgprPolicyName #-}

instance ToQuery PutGroupPolicy where
    toQuery = genericQuery def

data PutGroupPolicyResponse = PutGroupPolicyResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutGroupPolicy where
    type Sv PutGroupPolicy = IAM
    type Rs PutGroupPolicy = PutGroupPolicyResponse

    request = post "PutGroupPolicy"
    response _ = nullaryResponse PutGroupPolicyResponse
