{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.Service
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.StorageGateway.Service where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics               (Generic)
import Network.AWS.Internal
import Network.AWS.Internal.Types (Service(..))

-- | Currently supported version (@2013-06-30@) of the @AWS Storage Gateway@ service.
service :: Service
service = Service Global v4 "storagegateway" "2013-06-30"

jsonOptions :: Options
jsonOptions = defaultOptions

data StorageGatewayError
    = InternalServerError
    | InvalidGatewayRequestException
      deriving (Eq, Show, Generic)

instance FromJSON StorageGatewayError
