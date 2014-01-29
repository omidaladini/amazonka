{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.CancelUpdateStack
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Cancels an update on the specified stack. If the call completes
-- successfully, the stack will roll back the update and revert to the
-- previous stack configuration. Only stacks that are in the
-- UPDATE_IN_PROGRESS state can be canceled.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=CancelUpdateStack
-- &StackName=MyStack &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature].
module Network.AWS.CloudFormation.CancelUpdateStack where

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

import Network.AWS.CloudFormation.Service
import Network.AWS.CloudFormation.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
cancelUpdateStack :: Text
                  -> CancelUpdateStack
cancelUpdateStack p1 = undefined $ CancelUpdateStack
    { cusiStackName = p1
    }

data CancelUpdateStack = CancelUpdateStack
    { cusiStackName :: !Text
      -- ^ The name or the unique identifier associated with the stack.
    } deriving (Eq, Show, Generic)

instance ToQuery CancelUpdateStack

instance AWSRequest CancelUpdateStack where
    type Er CancelUpdateStack = CloudFormationError
    type Rs CancelUpdateStack = CancelUpdateStackResponse
    request = getQuery service "CancelUpdateStack"

data CancelUpdateStackResponse = CancelUpdateStackResponse
    deriving (Eq, Show, Generic)

instance FromXML CancelUpdateStackResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CancelUpdateStackResponse"
        :| ["CancelUpdateStackResult"]
