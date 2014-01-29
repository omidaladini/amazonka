{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.UpdateApp
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates a specified app. Required Permissions: To use this action, an IAM
-- user must have a Deploy or Manage permissions level for the stack, or an
-- attached policy that explicitly grants permissions. For more information on
-- user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.UpdateApp where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
updateApp :: Text
          -> UpdateApp
updateApp p1 = undefined $ UpdateApp
    { uarAppId = p1
    , uarAppSource = Nothing
    , uarAttributes = Map.empty
    , uarDescription = Nothing
    , uarDomains = []
    , uarEnableSsl = Nothing
    , uarName = Nothing
    , uarSslConfiguration = Nothing
    , uarType = Nothing
    }

data UpdateApp = UpdateApp
    { uarAppId :: !Text
      -- ^ The app ID.
    , uarAppSource :: Maybe Source
      -- ^ A Source object that specifies the app repository.
    , uarAttributes :: HashMap AppAttributesKeys Text
      -- ^ One or more user-defined key/value pairs to be added to the stack
      -- attributes bag.
    , uarDescription :: Maybe Text
      -- ^ A description of the app.
    , uarDomains :: [Text]
      -- ^ The app's virtual host settings, with multiple domains separated by commas.
      -- For example: 'www.example.com, example.com'.
    , uarEnableSsl :: Maybe Bool
      -- ^ Whether SSL is enabled for the app.
    , uarName :: Maybe Text
      -- ^ The app name.
    , uarSslConfiguration :: Maybe SslConfiguration
      -- ^ An SslConfiguration object with the SSL configuration.
    , uarType :: Maybe AppType
      -- ^ The app type.
    } deriving (Eq, Show, Generic)

instance ToJSON UpdateApp

instance AWSRequest UpdateApp where
    type Er UpdateApp = OpsWorksError
    type Rs UpdateApp = UpdateAppResponse
    request  = getJSON service
    response = responseJSON

data UpdateAppResponse = UpdateAppResponse
    deriving (Eq, Show, Generic)

instance FromJSON UpdateAppResponse
