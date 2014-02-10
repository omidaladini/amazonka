{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.CreateHealthCheck
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This action creates a new health check. To create a new health check, send
-- a POST request to the 2012-12-12/healthcheck resource. The request body
-- must include an XML document with a CreateHealthCheckRequest element. The
-- response returns the CreateHealthCheckResponse element that contains
-- metadata about the health check.
module Network.AWS.Route53.CreateHealthCheck where

import qualified Data.Text        as Text
import           Network.AWS.Core
import           Network.AWS.Route53.Service
import           Network.AWS.Route53.Types

data CreateHealthCheck = CreateHealthCheck
    { chcCallerReference :: !Text
      -- ^ A unique string that identifies the request and that allows failed
      -- CreateHealthCheck requests to be retried without the risk of executing the
      -- operation twice. You must use a unique CallerReference string every time
      -- you create a health check. CallerReference can be any unique string; you
      -- might choose to use a string that identifies your project. Valid characters
      -- are any Unicode code points that are legal in an XML 1.0 document. The
      -- UTF-8 encoding of the value must be less than 128 bytes.
    , chcHealthCheckConfig :: HealthCheckConfig
      -- ^ A complex type that contains health check configuration.
    } deriving (Eq, Show, Generic)

instance ToHeaders CreateHealthCheck

instance ToPath CreateHealthCheck where
    toPath = const "/2012-12-12/healthcheck"

instance ToQuery CreateHealthCheck where
    toQuery = const mempty

instance ToXML CreateHealthCheck where
    toXMLOptions = xmlOptions

instance AWSRequest CreateHealthCheck where
    type Er CreateHealthCheck = Route53Error
    type Rs CreateHealthCheck = CreateHealthCheckResponse
    request  = postRestXML service
    response = responseXML

data CreateHealthCheckResponse = CreateHealthCheckResponse
    { chcrHealthCheck :: HealthCheck
      -- ^ A complex type that contains identifying information about the health
      -- check.
    , chcrLocation :: !Text
      -- ^ The unique URL representing the new health check.
    } deriving (Eq, Show, Generic)

instance FromXML CreateHealthCheckResponse where
    fromXMLOptions = xmlOptions