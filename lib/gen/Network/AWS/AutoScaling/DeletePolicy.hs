{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DeletePolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a policy created by PutScalingPolicy.
module Network.AWS.AutoScaling.DeletePolicy where

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

import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deletePolicy :: ResourceName
             -> DeletePolicy
deletePolicy p1 = undefined $ DeletePolicy
    { dptPolicyName = p1
    , dptAutoScalingGroupName = Nothing
    }

data DeletePolicy = DeletePolicy
    { dptAutoScalingGroupName :: Maybe ResourceName
      -- ^ The name of the Auto Scaling group.
    , dptPolicyName :: !ResourceName
      -- ^ The name or PolicyARN of the policy you want to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeletePolicy

instance AWSRequest DeletePolicy where
    type Er DeletePolicy = AutoScalingError
    type Rs DeletePolicy = DeletePolicyResponse
    request = getQuery service "DeletePolicy"

data DeletePolicyResponse = DeletePolicyResponse
    deriving (Eq, Show, Generic)

instance FromXML DeletePolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeletePolicyResponse"
        :| ["DeletePolicyResult"]
