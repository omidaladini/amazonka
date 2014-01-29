{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.PutUserPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds (or updates) a policy document associated with the specified user. For
-- information about policies, refer to Overview of Policies in Using AWS
-- Identity and Access Management. For information about limits on the number
-- of policies you can associate with a user, see Limitations on IAM Entities
-- in Using AWS Identity and Access Management. Because policy documents can
-- be large, you should use POST rather than GET when calling PutUserPolicy.
-- For information about setting up signatures and authorization through the
-- API, go to Signing AWS API Requests in the AWS General Reference. For
-- general information about using the Query API with IAM, go to Making Query
-- Requests in Using IAM. https://iam.amazonaws.com/ ?Action=PutUserPolicy
-- &UserName=Bob &PolicyName=AllAccessPolicy
-- &PolicyDocument={"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":"*","Resource":"*"}]}
-- &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.PutUserPolicy where

import           Control.Monad
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import           Text.XML.Generic

import Network.AWS.IAM.Service
import Network.AWS.IAM.Types

-- | Convenience method utilising default fields where applicable.
putUserPolicy :: Text
              -> Text
              -> Text
              -> AWS (Either IAMError PutUserPolicyResponse)
putUserPolicy p1 p2 p3 = undefined $ PutUserPolicy
    { puprPolicyDocument = p1
    , puprPolicyName = p2
    , puprUserName = p3
    }

data PutUserPolicy = PutUserPolicy
    { puprPolicyDocument :: !Text
      -- ^ The policy document.
    , puprPolicyName :: !Text
      -- ^ Name of the policy document.
    , puprUserName :: !Text
      -- ^ Name of the user to associate the policy with.
    } deriving (Eq, Show, Generic)

instance ToQuery PutUserPolicy

instance AWSRequest PutUserPolicy where
    type Er PutUserPolicy = IAMError
    type Rs PutUserPolicy = PutUserPolicyResponse
    request = getQuery service "PutUserPolicy"

data PutUserPolicyResponse = PutUserPolicyResponse
    deriving (Eq, Show, Generic)

instance FromXML PutUserPolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "PutUserPolicyResponse"
        :| ["PutUserPolicyResult"]
