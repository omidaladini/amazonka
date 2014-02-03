{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.Service
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.SNS.Service where

import Data.Default
import Data.Tagged
import GHC.Generics               (Generic)
import Network.AWS.Internal
import Network.AWS.Internal.Types (Service(..))
import Text.XML.Generic

-- | Currently supported version (@2010-03-31@) of the @Amazon Simple Notification Service@ service.
service :: Service
service = Service Global v4 "sns" "2010-03-31"

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlInherit   = True
    , xmlNamespace = Just "http://sns.amazonaws.com/doc/2010-03-31/"
    }

data SNSError
    = AuthorizationErrorException
    | EndpointDisabledException
    | InternalErrorException
    | InvalidParameterException
    | NotFoundException
    | PlatformApplicationDisabledException
    | SubscriptionLimitExceededException
    | TopicLimitExceededException
      deriving (Eq, Show, Generic)

instance FromXML SNSError where
    fromXMLOptions = xmlOptions
