{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DeleteEventSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes an Amazon Redshift event notification subscription.
module Network.AWS.Redshift.DeleteEventSubscription where

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

import Network.AWS.Redshift.Service
import Network.AWS.Redshift.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteEventSubscription :: Text
                        -> DeleteEventSubscription
deleteEventSubscription p1 = undefined $ DeleteEventSubscription
    { desmSubscriptionName = p1
    }

data DeleteEventSubscription = DeleteEventSubscription
    { desmSubscriptionName :: !Text
      -- ^ The name of the Amazon Redshift event notification subscription to be
      -- deleted.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteEventSubscription

instance AWSRequest DeleteEventSubscription where
    type Er DeleteEventSubscription = RedshiftError
    type Rs DeleteEventSubscription = DeleteEventSubscriptionResponse
    request = getQuery service "DeleteEventSubscription"

data DeleteEventSubscriptionResponse = DeleteEventSubscriptionResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteEventSubscriptionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeleteEventSubscriptionResponse"
        :| ["DeleteEventSubscriptionResult"]
