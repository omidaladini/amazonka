{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.CreateApp
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an app for a specified stack. For more information, see Creating
-- Apps. Required Permissions: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see Managing User Permissions.
module Network.AWS.OpsWorks.CreateApp where

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
createApp :: Text
          -> Text
          -> AppType
          -> CreateApp
createApp p1 p2 p3 = CreateApp
    { carName = p1
    , carStackId = p2
    , carType = p3
    , carAppSource = Nothing
    , carAttributes = Map.empty
    , carDescription = Nothing
    , carDomains = []
    , carEnableSsl = Nothing
    , carShortname = Nothing
    , carSslConfiguration = Nothing
    }

data CreateApp = CreateApp
    { carAppSource :: Maybe Source
      -- ^ A Source object that specifies the app repository.
    , carAttributes :: HashMap AppAttributesKeys Text
      -- ^ One or more user-defined key/value pairs to be added to the stack
      -- attributes bag.
    , carDescription :: Maybe Text
      -- ^ A description of the app.
    , carDomains :: [Text]
      -- ^ The app virtual host settings, with multiple domains separated by commas.
      -- For example: 'www.example.com, example.com'.
    , carEnableSsl :: Maybe Bool
      -- ^ Whether to enable SSL for the app.
    , carName :: !Text
      -- ^ The app name.
    , carShortname :: Maybe Text
      -- ^ The app's short name.
    , carSslConfiguration :: Maybe SslConfiguration
      -- ^ An SslConfiguration object with the SSL configuration.
    , carStackId :: !Text
      -- ^ The stack ID.
    , carType :: !AppType
      -- ^ The app type. Each supported type is associated with a particular layer.
      -- For example, PHP applications are associated with a PHP layer. AWS OpsWorks
      -- deploys an application to those instances that are members of the
      -- corresponding layer.
    } deriving (Eq, Show, Generic)

instance ToJSON CreateApp where
    toJSON = genericToJSON jsonOptions

instance AWSRequest CreateApp where
    type Er CreateApp = OpsWorksError
    type Rs CreateApp = CreateAppResponse
    request  = getJSON service
    response = responseJSON

data CreateAppResponse = CreateAppResponse
    { carrsAppId :: Maybe Text
      -- ^ The app ID.
    } deriving (Eq, Show, Generic)

instance FromJSON CreateAppResponse where
    fromJSON = genericFromJSON jsonOptions

