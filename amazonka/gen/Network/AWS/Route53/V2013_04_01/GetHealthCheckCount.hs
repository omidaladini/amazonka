{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Route53.V2013_04_01.GetHealthCheckCount
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | To retrieve a count of all your health checks, send a GET request to the
-- 2013-04-01/healthcheckcount resource.
module Network.AWS.Route53.V2013_04_01.GetHealthCheckCount where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude

data GetHealthCheckCount = GetHealthCheckCount
    deriving (Eq, Show, Generic)

makeLenses ''GetHealthCheckCount

instance ToPath GetHealthCheckCount where
    toPath = const "/2013-04-01/healthcheckcount"

instance ToQuery GetHealthCheckCount

instance ToHeaders GetHealthCheckCount

instance ToXML GetHealthCheckCount where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetHealthCheckCountRequest"

data GetHealthCheckCountResponse = GetHealthCheckCountResponse
    { _ghccsHealthCheckCount :: Integer
      -- ^ The number of health checks associated with the current AWS
      -- account.
    } deriving (Show, Generic)

makeLenses ''GetHealthCheckCountResponse

instance AWSRequest GetHealthCheckCount where
    type Sv GetHealthCheckCount = Route53
    type Rs GetHealthCheckCount = GetHealthCheckCountResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure GetHealthCheckCountResponse
            <*> xml %| "HealthCheckCount"