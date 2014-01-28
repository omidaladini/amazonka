{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.GetHostnameSuggestion
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets a generated host name for the specified layer, based on the current
-- host name theme. Required Permissions: To use this action, an IAM user must
-- have a Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see Managing User Permissions.
module Network.AWS.OpsWorks.GetHostnameSuggestion where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.OpsWorks.Service
import Network.AWS.OpsWorks.Types

data GetHostnameSuggestion = GetHostnameSuggestion
    { ghsrLayerId :: !Text
      -- ^ The layer ID.
    } deriving (Eq, Show, Generic)

instance ToJSON GetHostnameSuggestion

instance AWSRequest GetHostnameSuggestion where
    type Er GetHostnameSuggestion = OpsWorksError
    type Rs GetHostnameSuggestion = GetHostnameSuggestionResponse
    request  = getJSON service
    response = responseJSON

data GetHostnameSuggestionResponse = GetHostnameSuggestionResponse
    { ghsrrsHostname :: Maybe Text
      -- ^ The generated host name.
    , ghsrrsLayerId :: Maybe Text
      -- ^ The layer ID.
    } deriving (Eq, Show, Generic)

instance FromJSON GetHostnameSuggestionResponse
