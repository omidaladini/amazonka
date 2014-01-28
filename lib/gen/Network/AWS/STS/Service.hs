{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.STS.Service
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.STS.Service where

import Data.Default
import Data.Tagged
import GHC.Generics               (Generic)
import Network.AWS.Internal
import Network.AWS.Internal.Types (Service(..))
import Text.XML.Generic

-- | Currently supported version (@2011-06-15@) of the @AWS Security Token Service@ service.
service :: Service
service = Service Global v4 "sts" "2011-06-15"

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlInherit   = True
    , xmlNamespace = Just "https://sts.amazonaws.com/doc/2011-06-15/"
    }

data STSError
    = ExpiredTokenException
    | IDPCommunicationErrorException
    | IDPRejectedClaimException
    | InvalidAuthorizationMessageException
    | InvalidIdentityTokenException
    | MalformedPolicyDocumentException
    | PackedPolicyTooLargeException
      deriving (Eq, Show, Generic)

instance FromXML STSError where
    fromXMLOptions = xmlOptions